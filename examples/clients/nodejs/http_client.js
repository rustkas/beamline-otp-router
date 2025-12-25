#!/usr/bin/env node
/**
 * Beamline Router HTTP Client (Node.js)
 * 
 * Demonstrates:
 * - HTTP requests to c-gateway
 * - Rate limit handling (429 + Retry-After)
 * - Exponential backoff retry
 * - Error handling
 * - API Key authentication
 */

const axios = require('axios');

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
 * HTTP client for Beamline Router via c-gateway
 */
class BeamlineHTTPClient {
    /**
     * Initialize HTTP client
     * 
     * @param {Object} options - Client options
     * @param {string} options.baseUrl - c-gateway base URL
     * @param {string} options.apiKey - API key for authentication
     * @param {string} options.tenantId - Tenant identifier
     * @param {RetryConfig} options.retryConfig - Retry configuration
     */
    constructor({
        baseUrl = 'http://localhost:8080',
        apiKey = null,
        tenantId = null,
        retryConfig = null
    } = {}) {
        this.baseUrl = baseUrl;
        this.apiKey = apiKey;
        this.tenantId = tenantId;
        this.retryConfig = retryConfig || new RetryConfig();

        // Create axios instance
        this.client = axios.create({
            baseURL: this.baseUrl,
            timeout: 5000,
            headers: {
                'Content-Type': 'application/json'
            }
        });
    }

    /**
     * Request a routing decision
     * 
     * @param {Object} options - Decision options
     * @param {string} options.messageType - Type of message
     * @param {string} options.payload - Message payload
     * @param {string} options.policyId - Policy identifier
     * @param {string} [options.messageId] - Message ID
     * @param {Object} [options.metadata] - Message metadata
     * @param {Object} [options.context] - Request context
     * @param {string} [options.runId] - Run ID
     * @param {string} [options.requestId] - Request ID
     * @param {string} [options.traceId] - Trace ID
     * @returns {Promise<Object>} Decision object
     */
    async decide({
        messageType,
        payload,
        policyId,
        messageId = null,
        metadata = null,
        context = null,
        runId = null,
        requestId = null,
        traceId = null
    }) {
        const headers = {
            'X-Tenant-ID': this.tenantId,
            'X-API-Key': this.apiKey
        };

        if (requestId) headers['X-Request-ID'] = requestId;
        if (traceId) headers['X-Trace-ID'] = traceId;

        const body = {
            version: '1',
            message: {
                type: messageType,
                payload: payload
            },
            policy_id: policyId
        };

        if (messageId) body.message.message_id = messageId;
        if (metadata) body.message.metadata = metadata;
        if (context) body.context = context;
        if (runId) body.run_id = runId;

        return this._requestWithRetry(async () => {
            return this.client.post('/api/v1/routes/decide', body, { headers });
        });
    }

    /**
     * Check c-gateway health
     * 
     * @returns {Promise<Object>} Health status
     */
    async health() {
        const response = await this.client.get('/_health');
        return response.data;
    }

    /**
     * Get Prometheus metrics
     * 
     * @returns {Promise<string>} Metrics in Prometheus text format
     */
    async metrics() {
        const response = await this.client.get('/metrics');
        return response.data;
    }

    /**
     * Execute HTTP request with exponential backoff retry
     * 
     * @param {Function} requestFunc - Function to execute
     * @returns {Promise<Object>} Response data
     * @private
     */
    async _requestWithRetry(requestFunc) {
        let lastError = null;

        for (let attempt = 1; attempt <= this.retryConfig.maxAttempts; attempt++) {
            try {
                const response = await requestFunc();

                // Check rate limiting (429)
                if (response.status === 429) {
                    const retryAfter = this._getRetryAfter(response);

                    if (attempt < this.retryConfig.maxAttempts) {
                        console.log(`Rate limit exceeded. Retrying after ${retryAfter}s...`);
                        await this._sleep(retryAfter * 1000);
                        continue;
                    } else {
                        throw new Error(`Rate limit exceeded: ${response.data.message || 'Too many requests'}`);
                    }
                }

                return response.data;

            } catch (error) {
                lastError = error;

                // Handle axios errors
                if (error.response) {
                    // HTTP error response
                    if (!this._isRetryableError(error)) {
                        throw error;
                    }

                    // Calculate backoff
                    if (attempt < this.retryConfig.maxAttempts) {
                        const delayMs = this._calculateBackoffDelay(attempt);
                        console.log(`Attempt ${attempt} failed: ${error.message}. Retrying in ${delayMs}ms...`);
                        await this._sleep(delayMs);
                    }
                } else if (error.request) {
                    // Network error (no response)
                    if (attempt < this.retryConfig.maxAttempts) {
                        const delayMs = this._calculateBackoffDelay(attempt);
                        console.log(`Network error on attempt ${attempt}: ${error.message}. Retrying in ${delayMs}ms...`);
                        await this._sleep(delayMs);
                    } else {
                        throw error;
                    }
                } else {
                    // Other error
                    throw error;
                }
            }
        }

        throw lastError;
    }

    /**
     * Get retry-after value from response headers
     * 
     * @param {Object} response - Axios response
     * @returns {number} Retry-after seconds (default: 60)
     * @private
     */
    _getRetryAfter(response) {
        const retryAfter = response.headers['retry-after'];
        if (retryAfter) {
            const parsed = parseInt(retryAfter, 10);
            return isNaN(parsed) ? 60 : parsed;
        }
        return 60;
    }

    /**
     * Calculate exponential backoff delay with jitter
     * 
     * @param {number} attempt - Current attempt number (1-indexed)
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
     * Check if error is retryable
     * 
     * @param {Error} error - Axios error
     * @returns {boolean} True if error is retryable
     * @private
     */
    _isRetryableError(error) {
        if (!error.response) {
            return true; // Network errors are retryable
        }

        const retryableCodes = [408, 429, 500, 502, 503, 504];
        return retryableCodes.includes(error.response.status);
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
}

/**
 * Example usage
 */
async function main() {
    console.log('=== Beamline Router HTTP Client Example (Node.js) ===\n');

    const client = new BeamlineHTTPClient({
        baseUrl: 'http://localhost:8080',
        apiKey: 'your-api-key',
        tenantId: 'demo-tenant',
        retryConfig: new RetryConfig({
            maxAttempts: 3,
            baseDelayMs: 200,
            maxDelayMs: 10000,
            jitter: 0.1
        })
    });

    try {
        // Check health
        console.log('Checking c-gateway health...');
        const health = await client.health();
        console.log(`Health: ${health.status}\n`);

        // Make routing decision
        console.log('Making routing decision...');
        const decision = await client.decide({
            messageType: 'text.generate',
            payload: 'Hello, world!',
            policyId: 'demo-policy',
            requestId: 'example-001',
            metadata: { model: 'gpt-4' }
        });

        console.log('✅ Decision received:');
        console.log(`   Provider: ${decision.provider_id}`);
        console.log(`   Reason: ${decision.reason}`);
        console.log(`   Priority: ${decision.priority || 'N/A'}`);
        console.log(`   Expected Cost: $${(decision.expected_cost || 0).toFixed(6)}`);

    } catch (error) {
        if (error.response) {
            console.error(`\n❌ HTTP Error: ${error.message}`);
            console.error(`   Status Code: ${error.response.status}`);
            console.error(`   Response: ${JSON.stringify(error.response.data)}`);
        } else if (error.request) {
            console.error(`\n❌ Network Error: ${error.message}`);
        } else {
            console.error(`\n❌ Error: ${error.message}`);
        }
        process.exit(1);
    }

    console.log('\nClient finished.');
}

// Run if called directly
if (require.main === module) {
    main();
}

module.exports = { BeamlineHTTPClient, RetryConfig };
