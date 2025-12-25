package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"math"
	"math/rand"
	"net/http"
	"strconv"
	"time"
)

// RetryConfig defines retry behavior configuration
type RetryConfig struct {
	MaxAttempts  int
	BaseDelayMs  int
	MaxDelayMs   int
	Jitter       float64
}

// DefaultRetryConfig returns default retry configuration
func DefaultRetryConfig() RetryConfig {
	return RetryConfig{
		MaxAttempts: 3,
		BaseDelayMs: 100,
		MaxDelayMs:  5000,
		Jitter:      0.1,
	}
}

// BeamlineHTTPClient is an HTTP client for c-gateway
type BeamlineHTTPClient struct {
	BaseURL     string
	APIKey      string
	TenantID    string
	RetryConfig RetryConfig
	HTTPClient  *http.Client
}

// NewBeamlineHTTPClient creates a new HTTP client
func NewBeamlineHTTPClient(baseURL, apiKey, tenantID string, retryConfig *RetryConfig) *BeamlineHTTPClient {
	config := DefaultRetryConfig()
	if retryConfig != nil {
		config = *retryConfig
	}

	return &BeamlineHTTPClient{
		BaseURL:     baseURL,
		APIKey:      apiKey,
		TenantID:    tenantID,
		RetryConfig: config,
		HTTPClient: &http.Client{
			Timeout: 5 * time.Second,
		},
	}
}

// DecideRequest represents a routing decision request
type DecideRequest struct {
	Version string                 `json:"version"`
	Message MessagePayload         `json:"message"`
	PolicyID string                `json:"policy_id"`
	Context map[string]interface{} `json:"context,omitempty"`
	RunID   string                 `json:"run_id,omitempty"`
}

// MessagePayload represents the message payload
type MessagePayload struct {
	MessageID string                 `json:"message_id,omitempty"`
	Type      string                 `json:"type"`
	Payload   string                 `json:"payload"`
	Metadata  map[string]interface{} `json:"metadata,omitempty"`
}

// DecideResponse represents a routing decision response
type DecideResponse struct {
	DecisionID       string                 `json:"decision_id"`
	ProviderID       string                 `json:"provider_id"`
	Reason           string                 `json:"reason"`
	Priority         int                    `json:"priority"`
	ExpectedCost     float64                `json:"expected_cost"`
	ExpectedLatency  int                    `json:"expected_latency_ms,omitempty"`
	Metadata         map[string]interface{} `json:"metadata,omitempty"`
}

// HealthResponse represents health check response
type HealthResponse struct {
	Status string `json:"status"`
	Reason string `json:"reason,omitempty"`
}

// Decide requests a routing decision
func (c *BeamlineHTTPClient) Decide(messageType, payload, policyID string, options map[string]interface{}) (*DecideResponse, error) {
	request := DecideRequest{
		Version: "1",
		Message: MessagePayload{
			Type:    messageType,
			Payload: payload,
		},
		PolicyID: policyID,
	}

	// Add optional fields
	if messageID, ok := options["message_id"].(string); ok {
		request.Message.MessageID = messageID
	}
	if metadata, ok := options["metadata"].(map[string]interface{}); ok {
		request.Message.Metadata = metadata
	}
	if context, ok := options["context"].(map[string]interface{}); ok {
		request.Context = context
	}
	if runID, ok := options["run_id"].(string); ok {
		request.RunID = runID
	}

	// Prepare request
	requestID, _ := options["request_id"].(string)
	traceID, _ := options["trace_id"].(string)

	var response DecideResponse
	err := c.requestWithRetry("POST", "/api/v1/routes/decide", request, &response, requestID, traceID)
	return &response, err
}

// Health checks c-gateway health
func (c *BeamlineHTTPClient) Health() (*HealthResponse, error) {
	var response HealthResponse
	err := c.doRequest("GET", "/_health", nil, &response, "", "")
	return &response, err
}

// Metrics retrieves Prometheus metrics
func (c *BeamlineHTTPClient) Metrics() (string, error) {
	req, err := http.NewRequest("GET", c.BaseURL+"/metrics", nil)
	if err != nil {
		return "", err
	}

	resp, err := c.HTTPClient.Do(req)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", err
	}

	return string(body), nil
}

// requestWithRetry executes HTTP request with exponential backoff retry
func (c *BeamlineHTTPClient) requestWithRetry(method, path string, body interface{}, result interface{}, requestID, traceID string) error {
	var lastErr error

	for attempt := 1; attempt <= c.RetryConfig.MaxAttempts; attempt++ {
		err := c.doRequest(method, path, body, result, requestID, traceID)
		if err == nil {
			return nil
		}

		lastErr = err

		// Check if error is retryable
		if httpErr, ok := err.(*HTTPError); ok {
			// Handle rate limiting (429)
			if httpErr.StatusCode == 429 {
				retryAfter := c.getRetryAfter(httpErr.Headers)
				if attempt < c.RetryConfig.MaxAttempts {
					fmt.Printf("Rate limit exceeded. Retrying after %ds...\n", retryAfter)
					time.Sleep(time.Duration(retryAfter) * time.Second)
					continue
				}
			}

			// Check if error is retryable
			if !c.isRetryableError(httpErr.StatusCode) {
				return err
			}
		}

		// Calculate backoff
		if attempt < c.RetryConfig.MaxAttempts {
			delayMs := c.calculateBackoffDelay(attempt)
			fmt.Printf("Attempt %d failed: %v. Retrying in %dms...\n", attempt, err, delayMs)
			time.Sleep(time.Duration(delayMs) * time.Millisecond)
		}
	}

	return lastErr
}

// doRequest executes HTTP request
func (c *BeamlineHTTPClient) doRequest(method, path string, body interface{}, result interface{}, requestID, traceID string) error {
	var reqBody io.Reader
	if body != nil {
		jsonBody, err := json.Marshal(body)
		if err != nil {
			return err
		}
		reqBody = bytes.NewBuffer(jsonBody)
	}

	req, err := http.NewRequest(method, c.BaseURL+path, reqBody)
	if err != nil {
		return err
	}

	// Set headers
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("X-Tenant-ID", c.TenantID)
	req.Header.Set("X-API-Key", c.APIKey)
	if requestID != "" {
		req.Header.Set("X-Request-ID", requestID)
	}
	if traceID != "" {
		req.Header.Set("X-Trace-ID", traceID)
	}

	resp, err := c.HTTPClient.Do(req)
	if err != nil {
		return err
	}
	defer resp.Body.Close()

	respBody, err := io.ReadAll(resp.Body)
	if err != nil {
		return err
	}

	// Check for HTTP errors
	if resp.StatusCode >= 400 {
		return &HTTPError{
			StatusCode: resp.StatusCode,
			Message:    string(respBody),
			Headers:    resp.Header,
		}
	}

	// Parse response
	if result != nil {
		if err := json.Unmarshal(respBody, result); err != nil {
			return err
		}
	}

	return nil
}

// getRetryAfter extracts Retry-After header value
func (c *BeamlineHTTPClient) getRetryAfter(headers http.Header) int {
	retryAfter := headers.Get("Retry-After")
	if retryAfter != "" {
		if seconds, err := strconv.Atoi(retryAfter); err == nil {
			return seconds
		}
	}
	return 60
}

// calculateBackoffDelay calculates exponential backoff delay with jitter
func (c *BeamlineHTTPClient) calculateBackoffDelay(attempt int) int {
	delayMs := math.Min(
		float64(c.RetryConfig.BaseDelayMs)*math.Pow(2, float64(attempt-1)),
		float64(c.RetryConfig.MaxDelayMs),
	)

	// Add jitter
	jitterRange := delayMs * c.RetryConfig.Jitter
	delayMs += (rand.Float64()*2 - 1) * jitterRange

	return int(math.Max(delayMs, 0))
}

// isRetryableError checks if HTTP status code is retryable
func (c *BeamlineHTTPClient) isRetryableError(statusCode int) bool {
	retryableCodes := []int{408, 429, 500, 502, 503, 504}
	for _, code := range retryableCodes {
		if statusCode == code {
			return true
		}
	}
	return false
}

// HTTPError represents an HTTP error
type HTTPError struct {
	StatusCode int
	Message    string
	Headers    http.Header
}

func (e *HTTPError) Error() string {
	return fmt.Sprintf("HTTP %d: %s", e.StatusCode, e.Message)
}

func main() {
	fmt.Println("=== Beamline Router HTTP Client Example (Go) ===\n")

	client := NewBeamlineHTTPClient(
		"http://localhost:8080",
		"your-api-key",
		"demo-tenant",
		&RetryConfig{
			MaxAttempts: 3,
			BaseDelayMs: 200,
			MaxDelayMs:  10000,
			Jitter:      0.1,
		},
	)

	// Check health
	fmt.Println("Checking c-gateway health...")
	health, err := client.Health()
	if err != nil {
		fmt.Printf("❌ Health check failed: %v\n", err)
		return
	}
	fmt.Printf("Health: %s\n\n", health.Status)

	// Make routing decision
	fmt.Println("Making routing decision...")
	decision, err := client.Decide(
		"text.generate",
		"Hello, world!",
		"demo-policy",
		map[string]interface{}{
			"request_id": "example-001",
			"metadata":   map[string]interface{}{"model": "gpt-4"},
		},
	)
	if err != nil {
		if httpErr, ok := err.(*HTTPError); ok {
			fmt.Printf("❌ HTTP Error: %v\n", httpErr)
			fmt.Printf("   Status Code: %d\n", httpErr.StatusCode)
		} else {
			fmt.Printf("❌ Error: %v\n", err)
		}
		return
	}

	fmt.Println("✅ Decision received:")
	fmt.Printf("   Provider: %s\n", decision.ProviderID)
	fmt.Printf("   Reason: %s\n", decision.Reason)
	fmt.Printf("   Priority: %d\n", decision.Priority)
	fmt.Printf("   Expected Cost: $%.6f\n", decision.ExpectedCost)
	fmt.Println("\nClient finished.")
}
