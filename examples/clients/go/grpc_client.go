package main

import (
	"context"
	"fmt"
	"math"
	"math/rand"
	"time"

	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/credentials/insecure"
	"google.golang.org/grpc/status"
)

// RetryConfig defines retry behavior configuration
type GRPCRetryConfig struct {
	MaxAttempts  int
	BaseDelayMs  int
	MaxDelayMs   int
	Jitter       float64
}

// DefaultGRPCRetryConfig returns default retry configuration
func DefaultGRPCRetryConfig() GRPCRetryConfig {
	return GRPCRetryConfig{
		MaxAttempts: 3,
		BaseDelayMs: 100,
		MaxDelayMs:  5000,
		Jitter:      0.1,
	}
}

// CircuitBreaker implements circuit breaker pattern
type CircuitBreaker struct {
	FailureThreshold      int
	RecoveryTimeoutSeconds int
	failures              int
	lastFailureTime       time.Time
	state                 string // CLOSED, OPEN, HALF_OPEN
}

// NewCircuitBreaker creates a new circuit breaker
func NewCircuitBreaker(failureThreshold, recoveryTimeoutSeconds int) *CircuitBreaker {
	return &CircuitBreaker{
		FailureThreshold:      failureThreshold,
		RecoveryTimeoutSeconds: recoveryTimeoutSeconds,
		state:                 "CLOSED",
	}
}

// Call executes function with circuit breaker protection
func (cb *CircuitBreaker) Call(fn func() error) error {
	if cb.state == "OPEN" {
		if time.Since(cb.lastFailureTime) > time.Duration(cb.RecoveryTimeoutSeconds)*time.Second {
			cb.state = "HALF_OPEN"
			fmt.Println("Circuit breaker: HALF_OPEN (attempting recovery)")
		} else {
			return fmt.Errorf("circuit breaker is OPEN - too many failures")
		}
	}

	err := fn()
	if err != nil {
		cb.failures++
		cb.lastFailureTime = time.Now()

		if cb.failures >= cb.FailureThreshold {
			cb.state = "OPEN"
			fmt.Printf("Circuit breaker: OPEN (failures: %d)\n", cb.failures)
		}
		return err
	}

	if cb.state == "HALF_OPEN" {
		cb.state = "CLOSED"
		cb.failures = 0
		fmt.Println("Circuit breaker: CLOSED (recovered)")
	}

	return nil
}

// BeamlineGRPCClient is a gRPC client for Router
type BeamlineGRPCClient struct {
	conn           *grpc.ClientConn
	retryConfig    GRPCRetryConfig
	circuitBreaker *CircuitBreaker
}

// NewBeamlineGRPCClient creates a new gRPC client
func NewBeamlineGRPCClient(address string, retryConfig *GRPCRetryConfig, useCircuitBreaker bool) (*BeamlineGRPCClient, error) {
	config := DefaultGRPCRetryConfig()
	if retryConfig != nil {
		config = *retryConfig
	}

	// Connect to gRPC server
	conn, err := grpc.Dial(address, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		return nil, fmt.Errorf("failed to connect: %w", err)
	}

	client := &BeamlineGRPCClient{
		conn:        conn,
		retryConfig: config,
	}

	if useCircuitBreaker {
		client.circuitBreaker = NewCircuitBreaker(5, 60)
	}

	return client, nil
}

// Close closes the gRPC connection
func (c *BeamlineGRPCClient) Close() error {
	if c.conn != nil {
		return c.conn.Close()
	}
	return nil
}

// DecideRequest represents a routing decision request
type GRPCDecideRequest struct {
	Version   string
	RequestID string
	TraceID   string
	TenantID  string
	PolicyID  string
	TaskType  string
	TaskPayload string
}

// DecideResponse represents a routing decision response
type GRPCDecideResponse struct {
	DecisionID  string
	ProviderID  string
	Reason      string
	Priority    int32
	ExpectedCost float64
}

// Decide requests a routing decision
func (c *BeamlineGRPCClient) Decide(req GRPCDecideRequest) (*GRPCDecideResponse, error) {
	// NOTE: This is a simplified example. In production, you would:
	// 1. Import the generated protobuf code
	// 2. Create a proper RouterServiceClient
	// 3. Call the actual Decide RPC method
	//
	// For demonstration, we show the retry and circuit breaker logic
	
	executeRequest := func() error {
		return c.decideWithRetry(req)
	}

	if c.circuitBreaker != nil {
		err := c.circuitBreaker.Call(executeRequest)
		if err != nil {
			return nil, err
		}
	} else {
		if err := executeRequest(); err != nil {
			return nil, err
		}
	}

	// This would come from actual gRPC response
	return &GRPCDecideResponse{
		DecisionID:  "dec-" + req.RequestID,
		ProviderID:  "openai",
		Reason:      "weighted_random",
		Priority:    1,
		ExpectedCost: 0.0001,
	}, nil
}

// decideWithRetry executes decision with exponential backoff retry
func (c *BeamlineGRPCClient) decideWithRetry(req GRPCDecideRequest) error {
	var lastErr error

	for attempt := 1; attempt <= c.retryConfig.MaxAttempts; attempt++ {
		// In production, make actual gRPC call here:
		// response, err := client.Decide(ctx, &pb.RouteRequest{...})
		
		// Simulate gRPC call
		err := c.simulateGRPCCall()
		if err == nil {
			return nil
		}

		lastErr = err

		// Check if error is retryable
		if !c.isRetryableError(err) {
			return err
		}

		// Calculate backoff
		if attempt < c.retryConfig.MaxAttempts {
			delayMs := c.calculateBackoffDelay(attempt)
			fmt.Printf("Attempt %d failed: %v. Retrying in %dms...\n", attempt, err, delayMs)
			time.Sleep(time.Duration(delayMs) * time.Millisecond)
		}
	}

	return lastErr
}

// simulateGRPCCall simulates a gRPC call (for demonstration)
func (c *BeamlineGRPCClient) simulateGRPCCall() error {
	// In production, this would be:
	// ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	// defer cancel()
	// response, err := client.Decide(ctx, request)
	
	// For demonstration, always succeed
	return nil
}

// calculateBackoffDelay calculates exponential backoff delay with jitter
func (c *BeamlineGRPCClient) calculateBackoffDelay(attempt int) int {
	delayMs := math.Min(
		float64(c.retryConfig.BaseDelayMs)*math.Pow(2, float64(attempt-1)),
		float64(c.retryConfig.MaxDelayMs),
	)

	// Add jitter
	jitterRange := delayMs * c.retryConfig.Jitter
	delayMs += (rand.Float64()*2 - 1) * jitterRange

	return int(math.Max(delayMs, 0))
}

// isRetryableError checks if gRPC error is retryable
func (c *BeamlineGRPCClient) isRetryableError(err error) bool {
	st, ok := status.FromError(err)
	if !ok {
		return true // Network errors are retryable
	}

	retryableCodes := []codes.Code{
		codes.Unavailable,
		codes.DeadlineExceeded,
		codes.ResourceExhausted,
		codes.Aborted,
	}

	for _, code := range retryableCodes {
		if st.Code() == code {
			return true
		}
	}

	return false
}

func main() {
	fmt.Println("=== Beamline Router gRPC Client Example (Go) ===\n")

	client, err := NewBeamlineGRPCClient(
		"localhost:50051",
		&GRPCRetryConfig{
			MaxAttempts: 3,
			BaseDelayMs: 100,
			MaxDelayMs:  5000,
			Jitter:      0.1,
		},
		true, // Use circuit breaker
	)
	if err != nil {
		fmt.Printf("❌ Failed to create client: %v\n", err)
		return
	}
	defer client.Close()

	// Make decision request
	fmt.Println("Making routing decision...")
	decision, err := client.Decide(GRPCDecideRequest{
		Version:     "1",
		RequestID:   "example-001",
		TraceID:     "trace-123",
		TenantID:    "demo-tenant",
		PolicyID:    "demo-policy",
		TaskType:    "text.generate",
		TaskPayload: "Hello, world!",
	})
	if err != nil {
		fmt.Printf("❌ Error: %v\n", err)
		return
	}

	fmt.Println("✅ Decision received:")
	fmt.Printf("   Provider: %s\n", decision.ProviderID)
	fmt.Printf("   Reason: %s\n", decision.Reason)
	fmt.Printf("   Priority: %d\n", decision.Priority)
	fmt.Printf("   Expected Cost: $%.6f\n", decision.ExpectedCost)
	fmt.Println("\nClient closed.")
}
