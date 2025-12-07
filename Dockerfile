# Build stage
FROM erlang:26-alpine AS builder

# Install build dependencies
RUN apk add --no-cache \
    git \
    curl \
    build-base

# Install rebar3
RUN curl -fsSL https://s3.amazonaws.com/rebar3/rebar3 -o /usr/local/bin/rebar3 && \
    chmod +x /usr/local/bin/rebar3

WORKDIR /build

# Copy rebar config and lock file
COPY rebar.config rebar.lock* ./

# Get dependencies
RUN rebar3 deps

# Copy source code
COPY src ./src
COPY include ./include
COPY priv ./priv

# Compile
RUN rebar3 compile

# Create release
## CP1: no relx release configured; run directly from compiled libs
## Build will copy compiled ebin dirs to runtime image

# Runtime stage
FROM erlang:26-alpine

# Install runtime dependencies
RUN apk add --no-cache \
    ncurses-libs \
    openssl \
    libstdc++

WORKDIR /app

# Copy compiled libs from builder
COPY --from=builder /build/_build/default/lib /app/lib

# Create non-root user
RUN addgroup -g 1000 router && \
    adduser -D -u 1000 -G router router && \
    chown -R router:router /app

USER router

# Expose gRPC port
EXPOSE 9000

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD pgrep -f "beamline_router" > /dev/null || exit 1

# Start application via erl from compiled ebin
CMD ["erl", "-pa", "/app/lib/*/ebin", "-eval", "application:ensure_all_started(beamline_router).", "-noshell"]


