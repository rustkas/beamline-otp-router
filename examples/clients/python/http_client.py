#!/usr/bin/env python3
"""
Beamline Router HTTP Client (via c-gateway)

Demonstrates:
- HTTP requests to c-gateway
- Rate limit handling (429 + Retry-After)
- Exponential backoff retry
- Error handling
- API Key authentication
"""

import requests
import time
import random
from typing import Dict, Optional
from dataclasses import dataclass


@dataclass
class RetryConfig:
    """Configuration for retry behavior"""
    max_attempts: int = 3
    base_delay_ms: int = 100
    max_delay_ms: int = 5000
    jitter: float = 0.1


class BeamlineHTTPClient:
    """HTTP client for Beamline Router via c-gateway"""
    
    def __init__(self,
                 base_url: str = "http://localhost:8080",
                 api_key: str = None,
                 tenant_id: str = None,
                 retry_config: Optional[RetryConfig] = None):
        """
        Initialize HTTP client
        
        Args:
            base_url: c-gateway base URL
            api_key: API key for authentication
            tenant_id: Tenant identifier
            retry_config: Retry configuration (optional)
        """
        self.base_url = base_url
        self.api_key = api_key
        self.tenant_id = tenant_id
        self.retry_config = retry_config or RetryConfig()
        self.session = requests.Session()
    
    def decide(self,
              message_type: str,
              payload: str,
              policy_id: str,
              message_id: Optional[str] = None,
              metadata: Optional[Dict] = None,
              context: Optional[Dict] = None,
              run_id: Optional[str] = None,
              request_id: Optional[str] = None,
              trace_id: Optional[str] = None) -> Dict:
        """
        Request a routing decision
        
        Args:
            message_type: Type of message (e.g., "text.generate")
            payload: Message payload
            policy_id: Policy identifier
            message_id: Message ID (optional)
            metadata: Message metadata (optional)
            context: Request context (optional)
            run_id: Run ID for tracking (optional)
            request_id: Request ID for tracking (optional)
            trace_id: Trace ID for distributed tracing (optional)
        
        Returns:
            Decision dictionary with provider_id, reason, etc.
        
        Raises:
            requests.HTTPError: On HTTP error
            requests.RequestException: On network error
        """
        headers = {
            "Content-Type": "application/json",
            "X-Tenant-ID": self.tenant_id,
            "X-API-Key": self.api_key
        }
        
        if request_id:
            headers["X-Request-ID"] = request_id
        if trace_id:
            headers["X-Trace-ID"] = trace_id
        
        body = {
            "version": "1",
            "message": {
                "type": message_type,
                "payload": payload
            },
            "policy_id": policy_id
        }
        
        if message_id:
            body["message"]["message_id"] = message_id
        if metadata:
            body["message"]["metadata"] = metadata
        if context:
            body["context"] = context
        if run_id:
            body["run_id"] = run_id
        
        return self._request_with_retry(
            lambda: self.session.post(
                f"{self.base_url}/api/v1/routes/decide",
                json=body,
                headers=headers,
                timeout=5
            )
        )
    
    def health(self) -> Dict:
        """
        Check c-gateway health
        
        Returns:
            Health status dictionary
        """
        response = self.session.get(
            f"{self.base_url}/_health",
            timeout=5
        )
        response.raise_for_status()
        return response.json()
    
    def metrics(self) -> str:
        """
        Get Prometheus metrics
        
        Returns:
            Metrics in Prometheus text format
        """
        response = self.session.get(
            f"{self.base_url}/metrics",
            timeout=5
        )
        response.raise_for_status()
        return response.text
    
    def _request_with_retry(self, request_func) -> Dict:
        """
        Execute HTTP request with exponential backoff retry
        
        Args:
            request_func: Function to execute
        
        Returns:
            Response JSON
        
        Raises:
            requests.HTTPError: On HTTP error after retries
        """
        last_exception = None
        
        for attempt in range(1, self.retry_config.max_attempts + 1):
            try:
                response = request_func()
                
                # Handle rate limiting (429)
                if response.status_code == 429:
                    retry_after = self._get_retry_after(response)
                    
                    if attempt < self.retry_config.max_attempts:
                        print(f"Rate limit exceeded. Retrying after {retry_after}s...")
                        time.sleep(retry_after)
                        continue
                    else:
                        response.raise_for_status()
                
                # Raise for other errors
                response.raise_for_status()
                
                return response.json()
            
            except requests.HTTPError as e:
                last_exception = e
                
                # Check if error is retryable
                if not self._is_retryable_error(e):
                    raise
                
                # Calculate backoff with jitter
                if attempt < self.retry_config.max_attempts:
                    delay_ms = self._calculate_backoff_delay(attempt)
                    
                    print(f"Attempt {attempt} failed: {e}. Retrying in {delay_ms:.0f}ms...")
                    time.sleep(delay_ms / 1000.0)
            
            except requests.RequestException as e:
                last_exception = e
                
                # Network errors are retryable
                if attempt < self.retry_config.max_attempts:
                    delay_ms = self._calculate_backoff_delay(attempt)
                    
                    print(f"Network error on attempt {attempt}: {e}. Retrying in {delay_ms:.0f}ms...")
                    time.sleep(delay_ms / 1000.0)
                else:
                    raise
        
        raise last_exception
    
    def _get_retry_after(self, response: requests.Response) -> int:
        """
        Get retry-after value from response headers
        
        Args:
            response: HTTP response
        
        Returns:
            Retry-after seconds (default: 60)
        """
        retry_after = response.headers.get('Retry-After', '60')
        try:
            return int(retry_after)
        except ValueError:
            return 60
    
    def _calculate_backoff_delay(self, attempt: int) -> float:
        """
        Calculate exponential backoff delay with jitter
        
        Args:
            attempt: Current attempt number (1-indexed)
        
        Returns:
            Delay in milliseconds
        """
        delay_ms = min(
            self.retry_config.base_delay_ms * (2 ** (attempt - 1)),
            self.retry_config.max_delay_ms
        )
        
        # Add jitter
        jitter_range = delay_ms * self.retry_config.jitter
        delay_ms += random.uniform(-jitter_range, jitter_range)
        
        return max(delay_ms, 0)
    
    @staticmethod
    def _is_retryable_error(error: requests.HTTPError) -> bool:
        """
        Check if HTTP error is retryable
        
        Args:
            error: HTTP error
        
        Returns:
            True if error is retryable
        """
        if error.response is None:
            return True
        
        # Retryable status codes
        retryable_codes = {
            408,  # Request Timeout
            429,  # Too Many Requests
            500,  # Internal Server Error
            502,  # Bad Gateway
            503,  # Service Unavailable
            504,  # Gateway Timeout
        }
        
        return error.response.status_code in retryable_codes
    
    def close(self):
        """Close HTTP session"""
        if self.session:
            self.session.close()


def main():
    """Example usage"""
    print("=== Beamline Router HTTP Client Example ===\n")
    
    # Create client
    client = BeamlineHTTPClient(
        base_url="http://localhost:8080",
        api_key="your-api-key",
        tenant_id="demo-tenant",
        retry_config=RetryConfig(
            max_attempts=3,
            base_delay_ms=200,
            max_delay_ms=10000,
            jitter=0.1
        )
    )
    
    try:
        # Check health
        print("Checking c-gateway health...")
        health = client.health()
        print(f"Health: {health['status']}\n")
        
        # Make routing decision
        print("Making routing decision...")
        decision = client.decide(
            message_type="text.generate",
            payload="Hello, world!",
            policy_id="demo-policy",
            request_id="example-001",
            metadata={"model": "gpt-4"}
        )
        
        print(f"✅ Decision received:")
        print(f"   Provider: {decision['provider_id']}")
        print(f"   Reason: {decision['reason']}")
        print(f"   Priority: {decision.get('priority', 'N/A')}")
        print(f"   Expected Cost: ${decision.get('expected_cost', 0):.6f}")
        
    except requests.HTTPError as e:
        print(f"\n❌ HTTP Error: {e}")
        if e.response is not None:
            print(f"   Status Code: {e.response.status_code}")
            print(f"   Response: {e.response.text}")
    
    except requests.RequestException as e:
        print(f"\n❌ Network Error: {e}")
    
    finally:
        client.close()
        print("\nClient closed.")


if __name__ == "__main__":
    main()
