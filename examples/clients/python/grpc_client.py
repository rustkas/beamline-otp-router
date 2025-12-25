#!/usr/bin/env python3
"""
Beamline Router gRPC Client Example (Python)

Demonstrates:
- Basic gRPC calls
- Retry with exponential backoff
- Circuit breaker pattern
- Error handling
"""

import grpc
import time
import random
from typing import Optional
from dataclasses import dataclass

# Import generated protobuf files
# Assuming you've run: python -m grpc_tools.protoc -I../../proto --python_out=. --grpc_python_out=. ../../proto/beamline/flow/v1/flow.proto
# from beamline.flow.v1 import flow_pb2, flow_pb2_grpc


@dataclass
class RetryConfig:
    """Configuration for retry behavior"""
    max_attempts: int = 3
    base_delay_ms: int = 100
    max_delay_ms: int = 5000
    jitter: float = 0.1


class CircuitBreaker:
    """Simple circuit breaker implementation"""
    
    def __init__(self, failure_threshold: int = 5, recovery_timeout: float = 60.0):
        self.failure_threshold = failure_threshold
        self.recovery_timeout = recovery_timeout
        self.failures = 0
        self.last_failure_time: Optional[float] = None
        self.state = "CLOSED"  # CLOSED, OPEN, HALF_OPEN
    
    def call(self, func, *args, **kwargs):
        """Execute function with circuit breaker protection"""
        if self.state == "OPEN":
            if time.time() - self.last_failure_time > self.recovery_timeout:
                self.state = "HALF_OPEN"
                print("Circuit breaker: HALF_OPEN (attempting recovery)")
            else:
                raise Exception("Circuit breaker is OPEN - too many failures")
        
        try:
            result = func(*args, **kwargs)
            if self.state == "HALF_OPEN":
                self.state = "CLOSED"
                self.failures = 0
                print("Circuit breaker: CLOSED (recovered)")
            return result
        except Exception as e:
            self.failures += 1
            self.last_failure_time = time.time()
            
            if self.failures >= self.failure_threshold:
                self.state = "OPEN"
                print(f"Circuit breaker: OPEN (failures: {self.failures})")
            
            raise


class BeamlineRouterClient:
    """Beamline Router gRPC client with retry and circuit breaker"""
    
    def __init__(self, 
                 host: str = "localhost",
                 port: int = 50051,
                 retry_config: Optional[RetryConfig] = None,
                 use_circuit_breaker: bool = True):
        self.address = f"{host}:{port}"
        self.retry_config = retry_config or RetryConfig()
        self.circuit_breaker = CircuitBreaker() if use_circuit_breaker else None
        
        # Create gRPC channel
        self.channel = grpc.insecure_channel(self.address)
        # self.stub = flow_pb2_grpc.RouterServiceStub(self.channel)
    
    def close(self):
        """Close the gRPC channel"""
        if self.channel:
            self.channel.close()
    
    def decide(self, 
               tenant_id: str,
               policy_id: str,
               task_type: str,
               task_payload: str,
               request_id: Optional[str] = None,
               trace_id: Optional[str] = None) -> dict:
        """
        Make a routing decision
        
        Args:
            tenant_id: Tenant identifier
            policy_id: Policy identifier
            task_type: Type of task (e.g., "text.generate")
            task_payload: Task payload
            request_id: Optional request ID for tracing
            trace_id: Optional trace ID for distributed tracing
        
        Returns:
            Decision dictionary with provider_id, reason, etc.
        """
        # Create request
        request = {
            "version": "1",
            "request_id": request_id or f"req-{int(time.time() * 1000)}",
            "trace_id": trace_id or f"trace-{int(time.time() * 1000)}",
            "tenant_id": tenant_id,
            "policy_id": policy_id,
            "task": {
                "type": task_type,
                "payload": task_payload
            }
        }
        
        # Execute with retry and circuit breaker
        if self.circuit_breaker:
            return self.circuit_breaker.call(self._decide_with_retry, request)
        else:
            return self._decide_with_retry(request)
    
    def _decide_with_retry(self, request: dict) -> dict:
        """Execute decision with exponential backoff retry"""
        last_exception = None
        
        for attempt in range(1, self.retry_config.max_attempts + 1):
            try:
                # Make gRPC call
                # response = self.stub.Decide(flow_pb2.RouteRequest(**request))
                
                # Simulate gRPC call for demo
                response = self._simulate_decide_call(request)
                
                return {
                    "decision_id": response.get("decision_id"),
                    "provider_id": response.get("provider_id"),
                    "reason": response.get("reason"),
                    "priority": response.get("priority", 0),
                    "expected_cost": response.get("expected_cost", 0.0)
                }
            
            except grpc.RpcError as e:
                last_exception = e
                
                # Check if error is retryable
                if not self._is_retryable_error(e):
                    raise
                
                # Calculate backoff with jitter
                if attempt < self.retry_config.max_attempts:
                    delay_ms = min(
                        self.retry_config.base_delay_ms * (2 ** (attempt - 1)),
                        self.retry_config.max_delay_ms
                    )
                    
                    # Add jitter
                    jitter_range = delay_ms * self.retry_config.jitter
                    delay_ms += random.uniform(-jitter_range, jitter_range)
                    
                    print(f"Attempt {attempt} failed: {e.code().name}. Retrying in {delay_ms:.0f}ms...")
                    time.sleep(delay_ms / 1000.0)
        
        raise last_exception
    
    def _simulate_decide_call(self, request: dict) -> dict:
        """Simulate gRPC call for demo purposes"""
        # Simulate occasional failures
        if random.random() < 0.1:  # 10% failure rate
            raise grpc.RpcError("Simulated transient error")
        
        return {
            "decision_id": f"dec-{int(time.time() * 1000)}",
            "provider_id": "openai",
            "reason": "weighted_random",
            "priority": 1,
            "expected_cost": 0.0001
        }
    
    @staticmethod
    def _is_retryable_error(error: grpc.RpcError) -> bool:
        """Check if gRPC error is retryable"""
        retryable_codes = {
            grpc.StatusCode.UNAVAILABLE,
            grpc.StatusCode.DEADLINE_EXCEEDED,
            grpc.StatusCode.RESOURCE_EXHAUSTED,
            grpc.StatusCode.ABORTED,
        }
        return error.code() in retryable_codes


def main():
    """Example usage"""
    print("=== Beamline Router Python Client Example ===\n")
    
    # Create client
    client = BeamlineRouterClient(
        host="localhost",
        port=50051,
        retry_config=RetryConfig(
            max_attempts=3,
            base_delay_ms=100,
            max_delay_ms=5000,
            jitter=0.1
        ),
        use_circuit_breaker=True
    )
    
    try:
        # Make decision request
        print("Making routing decision...")
        decision = client.decide(
            tenant_id="demo-tenant",
            policy_id="demo-policy",
            task_type="text.generate",
            task_payload="Hello, world!",
            request_id="example-001"
        )
        
        print(f"\n✅ Decision received:")
        print(f"   Provider: {decision['provider_id']}")
        print(f"   Reason: {decision['reason']}")
        print(f"   Priority: {decision['priority']}")
        print(f"   Expected Cost: ${decision['expected_cost']:.6f}")
        
    except Exception as e:
        print(f"\n❌ Error: {e}")
    
    finally:
        client.close()
        print("\nClient closed.")


if __name__ == "__main__":
    main()
