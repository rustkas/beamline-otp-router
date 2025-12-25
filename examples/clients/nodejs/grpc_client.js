#!/usr/bin/env node
/**
 * Beamline Router gRPC Client (Node.js)
 * 
 * Demonstrates:
 * - gRPC calls to Router
 * - Exponential backoff retry
 * - Circuit breaker pattern
 * - Error handling
 */

const grpc = require('@grpc/grpc-js');
const protoLoader = require('@grpc/proto-loader');
const path = require('path');

/**
 * Retry configuration
 */
class RetryConfig {
    constructor({
        maxAttempts = 3,
        baseDelayMs = 100,
        maxDelayMs = 5000,
        jitter = 0.1
    } = {}) {
        this.maxAttempts = maxAttempts;
        this.baseDelayMs = baseDelayMs;
        this.maxDelayMs = maxDelayMs;
        this.jitter = jitter;
    }
}

/**
 * Circuit breaker implementation
 */
class CircuitBreaker {
    constructor({
        failureThreshold = 5,
        recoveryTimeoutSeconds = 60
    } = {}) {
        this.failureThreshold = failureThreshold;
        this.recoveryTimeoutSeconds = recoveryTimeoutSeconds;
        this.failures = 0;
        this.lastFailureTime = null;
        this.state = 'CLOSED'; // CLOSED, OPEN, HALF_OPEN
    }

    /**
     * Execute function with circuit breaker protection
     * 
     * @param {Function} func - Function to execute
     * @returns {Promise<*>} Function result
     */
    async call(func) {
        if (this.state === 'OPEN') {
            const now = Date.now();
            if (now - this.lastFailureTime > this.recoveryTimeoutSeconds * 1000) {
                this.state = 'HALF_OPEN';
                console.log('Circuit breaker: HALF_OPEN (attempting recovery)');
            } else {
                throw new Error('Circuit breaker is OPEN - too many failures');
            }
        }

        try {
            const result = await func();

            if (this.state === 'HALF_OPEN') {
                this.state = 'CLOSED';
                this.failures = 0;
                console.log('Circuit breaker: CLOSED (recovered)');
            }

            return result;
        } catch (error) {
            this.failures++;
            this.lastFailureTime = Date.now();

            if (this.failures >= this.failureThreshold) {
                this.state = 'OPEN';
                console.log(`Circuit breaker: OPEN (failures: ${this.failures})`);
            }

            throw error;
        }
    }
}

/**
 * gRPC client for Beamline Router
 */
class BeamlineGRPCClient {
    /**
     * Initialize gRPC client
     * 
     * @param {Object} options - Client options
     * @param {string} options.host - Router host
     * @param {number} options.port - Router port
     * @param {string} options.protoPath - Path to proto file
     * @param {RetryConfig} options.retryConfig - Retry configuration
     * @param {boolean} options.useCircuitBreaker - Enable circuit breaker
     */
    constructor({
        host = 'localhost',
        port = 50051,
        protoPath = null,
        retryConfig = null,
        useCircuitBreaker = true
    } = {}) {
        this.host = host;
        this.port = port;
        this.address = `${host}:${port}`;
        this.retryConfig = retryConfig || new RetryConfig();
        this.circuitBreaker = useCircuitBreaker ? new CircuitBreaker() : null;

        // Load proto file
        if (!protoPath) {
            protoPath = path.join(__dirname, '../../../proto/beamline/flow/v1/flow.proto');
        }

        const packageDefinition = protoLoader.loadSync(protoPath, {
            keepCase: true,
            longs: String,
            enums: String,
            defaults: true,
            oneofs: true
        });

        const protoDescriptor = grpc.loadPackageDefinition(packageDefinition);
        const RouterService = protoDescriptor.beamline.flow.v1.RouterService;

        // Create gRPC client
        this.client = new RouterService(
            this.address,
            grpc.credentials.createInsecure()
        );
    }

    /**
     * Request a routing decision
     * 
     * @param {Object} options - Decision options
     * @param {string} options.tenantId - Tenant identifier
     * @param {string} options.policyId - Policy identifier
     * @param {string} options.taskType - Task type
     * @param {string} options.taskPayload - Task payload
     * @param {string} [options.requestId] - Request ID
     * @param {string} [options.traceId] - Trace ID
     * @returns {Promise<Object>} Decision object
     */
    async decide({
        tenantId,
        policyId,
        taskType,
        taskPayload,
        requestId = null,
        traceId = null
    }) {
        const request = {
            version: '1',
            request_id: requestId || `req-${Date.now()}`,
            trace_id: traceId || `trace-${Date.now()}`,
            tenant_id: tenantId,
            policy_id: policyId,
            task: {
                type: taskType,
                payload: taskPayload
            }
        };

        // Execute with retry and circuit breaker
        const executeRequest = async () => {
            return this._decideWithRetry(request);
        };

        if (this.circuitBreaker) {
            return this.circuitBreaker.call(executeRequest);
        } else {
            return executeRequest();
        }
    }

    /**
     * Execute decision with exponential backoff retry
     * 
     * @param {Object} request - gRPC request
     * @returns {Promise<Object>} Decision object
     * @private
     */
    async _decideWithRetry(request) {
        let lastError = null;

        for (let attempt = 1; attempt <= this.retryConfig.maxAttempts; attempt++) {
            try {
                // Make gRPC call
                const response = await this._grpcCall('Decide', request);

                return {
                    decision_id: response.decision_id,
                    provider_id: response.provider_id,
                    reason: response.reason,
                    priority: response.priority || 0,
                    expected_cost: response.expected_cost || 0.0
                };

            } catch (error) {
                lastError = error;

                // Check if error is retryable
                if (!this._isRetryableError(error)) {
                    throw error;
                }

                // Calculate backoff
                if (attempt < this.retryConfig.maxAttempts) {
                    const delayMs = this._calculateBackoffDelay(attempt);
                    console.log(`Attempt ${attempt} failed: ${error.details || error.message}. Retrying in ${delayMs}ms...`);
                    await this._sleep(delayMs);
                }
            }
        }

        throw lastError;
    }

    /**
     * Make gRPC call
     * 
     * @param {string} method - Method name
     * @param {Object} request - Request object
     * @returns {Promise<Object>} Response object
     * @private
     */
    _grpcCall(method, request) {
        return new Promise((resolve, reject) => {
            this.client[method](request, (error, response) => {
                if (error) {
                    reject(error);
                } else {
                    resolve(response);
                }
            });
        });
    }

    /**
     * Calculate exponential backoff delay with jitter
     * 
     * @param {number} attempt - Current attempt number
     * @returns {number} Delay in milliseconds
     * @private
     */
    _calculateBackoffDelay(attempt) {
        let delayMs = Math.min(
            this.retryConfig.baseDelayMs * Math.pow(2, attempt - 1),
            this.retryConfig.maxDelayMs
        );

        // Add jitter
        const jitterRange = delayMs * this.retryConfig.jitter;
        delayMs += (Math.random() * 2 - 1) * jitterRange;

        return Math.max(delayMs, 0);
    }

    /**
     * Check if gRPC error is retryable
     * 
     * @param {Error} error - gRPC error
     * @returns {boolean} True if error is retryable
     * @private
     */
    _isRetryableError(error) {
        const retryableCodes = [
            grpc.status.UNAVAILABLE,
            grpc.status.DEADLINE_EXCEEDED,
            grpc.status.RESOURCE_EXHAUSTED,
            grpc.status.ABORTED
        ];

        return retryableCodes.includes(error.code);
    }

    /**
     * Sleep for specified milliseconds
     * 
     * @param {number} ms - Milliseconds to sleep
     * @returns {Promise<void>}
     * @private
     */
    _sleep(ms) {
        return new Promise(resolve => setTimeout(resolve, ms));
    }

    /**
     * Close gRPC client
     */
    close() {
        if (this.client) {
            this.client.close();
        }
    }
}

/**
 * Example usage
 */
async function main() {
    console.log('=== Beamline Router gRPC Client Example (Node.js) ===\n');

    const client = new BeamlineGRPCClient({
        host: 'localhost',
        port: 50051,
        retryConfig: new RetryConfig({
            maxAttempts: 3,
            baseDelayMs: 100,
            maxDelayMs: 5000,
            jitter: 0.1
        }),
        useCircuitBreaker: true
    });

    try {
        // Make decision request
        console.log('Making routing decision...');
        const decision = await client.decide({
            tenantId: 'demo-tenant',
            policyId: 'demo-policy',
            taskType: 'text.generate',
            taskPayload: 'Hello, world!',
            requestId: 'example-001'
        });

        console.log('✅ Decision received:');
        console.log(`   Provider: ${decision.provider_id}`);
        console.log(`   Reason: ${decision.reason}`);
        console.log(`   Priority: ${decision.priority}`);
        console.log(`   Expected Cost: $${decision.expected_cost.toFixed(6)}`);

    } catch (error) {
        console.error(`\n❌ Error: ${error.details || error.message}`);
        if (error.code) {
            console.error(`   gRPC Code: ${error.code}`);
        }
        process.exit(1);
    } finally {
        client.close();
        console.log('\nClient closed.');
    }
}

// Run if called directly
if (require.main === module) {
    main();
}

module.exports = { BeamlineGRPCClient, RetryConfig, CircuitBreaker };
