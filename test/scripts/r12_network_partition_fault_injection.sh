#!/bin/bash
# R12: Network Partition Fault Injection Script
# 
# Purpose: Scripts for reproducing network partition scenarios
# Supports: Local environment (docker-compose, bare metal)
# 
# Usage:
#   ./r12_network_partition_fault_injection.sh create <partition_type> <from> <to> [options]
#   ./r12_network_partition_fault_injection.sh remove <partition_id>
#   ./r12_network_partition_fault_injection.sh list
#   ./r12_network_partition_fault_injection.sh status <partition_id>
#   ./r12_network_partition_fault_injection.sh heal <partition_id>
#   ./r12_network_partition_fault_injection.sh flapping <from> <to> <interval_ms> <duration_ms>
#
# Examples:
#   # Create single-instance partition (Router -> NATS)
#   ./r12_network_partition_fault_injection.sh create single_instance router nats --action drop
#
#   # Create multi-instance split-brain
#   ./r12_network_partition_fault_injection.sh create multi_instance router-group-1 router-group-2 --action drop
#
#   # Create partition with delay
#   ./r12_network_partition_fault_injection.sh create single_instance router nats --action delay --delay_ms 5000
#
#   # Simulate flapping network
#   ./r12_network_partition_fault_injection.sh flapping router nats 2000 1000

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
NETWORK_PARTITION_REAL="${NETWORK_PARTITION_REAL:-false}"

# Partition storage (for tracking)
PARTITION_STORAGE_DIR="${SCRIPT_DIR}/.partitions"
mkdir -p "${PARTITION_STORAGE_DIR}"

# Logging
log_info() {
    echo -e "${GREEN}[INFO]${NC} $*"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*" >&2
}

# Check if running with required privileges
check_privileges() {
    if [ "${NETWORK_PARTITION_REAL}" = "true" ]; then
        if [ "$EUID" -ne 0 ]; then
            log_error "Real network tools mode requires root privileges"
            log_error "Run with: sudo $0 $*"
            exit 1
        fi
    fi
}

# Check if network tools are available
check_network_tools() {
    if [ "${NETWORK_PARTITION_REAL}" = "true" ]; then
        if ! command -v iptables &> /dev/null; then
            log_error "iptables not found. Install iptables or use mock mode."
            exit 1
        fi
        if ! command -v tc &> /dev/null; then
            log_warn "tc (traffic control) not found. Delay/loss actions may not work."
        fi
    fi
}

# Generate partition ID
generate_partition_id() {
    echo "partition-$(date +%s)-$$"
}

# Create partition (real mode)
create_partition_real() {
    local partition_type="$1"
    local from="$2"
    local to="$3"
    local action="${4:-drop}"
    local delay_ms="${5:-0}"
    local loss_percent="${6:-0}"
    
    local partition_id=$(generate_partition_id)
    
    log_info "Creating partition (real mode): ${partition_id}"
    log_info "  Type: ${partition_type}"
    log_info "  From: ${from}"
    log_info "  To: ${to}"
    log_info "  Action: ${action}"
    
    case "${action}" in
        drop)
            # Use iptables to drop packets
            if [ "${NETWORK_PARTITION_REAL}" = "true" ]; then
                # Resolve hostnames to IPs (simplified)
                FROM_IP=$(getent hosts "${from}" | awk '{print $1}' | head -1 || echo "${from}")
                TO_IP=$(getent hosts "${to}" | awk '{print $1}' | head -1 || echo "${to}")
                
                # Create iptables rule
                iptables -A OUTPUT -d "${TO_IP}" -j DROP 2>/dev/null || true
                iptables -A INPUT -s "${TO_IP}" -j DROP 2>/dev/null || true
                
                log_info "iptables rules added for ${FROM_IP} -> ${TO_IP}"
            fi
            ;;
        delay)
            if [ "${NETWORK_PARTITION_REAL}" = "true" ] && command -v tc &> /dev/null; then
                # Use tc/netem to add delay
                INTERFACE=$(ip route | grep default | awk '{print $5}' | head -1)
                if [ -n "${INTERFACE}" ]; then
                    tc qdisc add dev "${INTERFACE}" root netem delay "${delay_ms}ms" 2>/dev/null || true
                    log_info "tc delay added: ${delay_ms}ms on ${INTERFACE}"
                fi
            fi
            ;;
        reject)
            if [ "${NETWORK_PARTITION_REAL}" = "true" ]; then
                FROM_IP=$(getent hosts "${from}" | awk '{print $1}' | head -1 || echo "${from}")
                TO_IP=$(getent hosts "${to}" | awk '{print $1}' | head -1 || echo "${to}")
                
                iptables -A OUTPUT -d "${TO_IP}" -j REJECT 2>/dev/null || true
                log_info "iptables REJECT rule added for ${FROM_IP} -> ${TO_IP}"
            fi
            ;;
        loss)
            if [ "${NETWORK_PARTITION_REAL}" = "true" ] && command -v tc &> /dev/null; then
                INTERFACE=$(ip route | grep default | awk '{print $5}' | head -1)
                if [ -n "${INTERFACE}" ]; then
                    tc qdisc add dev "${INTERFACE}" root netem loss "${loss_percent}%" 2>/dev/null || true
                    log_info "tc packet loss added: ${loss_percent}% on ${INTERFACE}"
                fi
            fi
            ;;
        *)
            log_error "Unsupported action: ${action}"
            exit 1
            ;;
    esac
    
    # Store partition info
    cat > "${PARTITION_STORAGE_DIR}/${partition_id}.json" <<EOF
{
  "partition_id": "${partition_id}",
  "type": "${partition_type}",
  "from": "${from}",
  "to": "${to}",
  "action": "${action}",
  "delay_ms": "${delay_ms}",
  "loss_percent": "${loss_percent}",
  "created_at": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "mode": "real"
}
EOF
    
    echo "${partition_id}"
}

# Create partition (mock mode)
create_partition_mock() {
    local partition_type="$1"
    local from="$2"
    local to="$3"
    local action="${4:-drop}"
    
    local partition_id=$(generate_partition_id)
    
    log_info "Creating partition (mock mode): ${partition_id}"
    log_info "  Type: ${partition_type}"
    log_info "  From: ${from}"
    log_info "  To: ${to}"
    log_info "  Action: ${action}"
    log_warn "Mock mode: Partition simulated, no real network changes"
    
    # Store partition info
    cat > "${PARTITION_STORAGE_DIR}/${partition_id}.json" <<EOF
{
  "partition_id": "${partition_id}",
  "type": "${partition_type}",
  "from": "${from}",
  "to": "${to}",
  "action": "${action}",
  "created_at": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "mode": "mock"
}
EOF
    
    echo "${partition_id}"
}

# Remove partition
remove_partition() {
    local partition_id="$1"
    local partition_file="${PARTITION_STORAGE_DIR}/${partition_id}.json"
    
    if [ ! -f "${partition_file}" ]; then
        log_error "Partition not found: ${partition_id}"
        exit 1
    fi
    
    local mode=$(jq -r '.mode' "${partition_file}" 2>/dev/null || echo "mock")
    
    if [ "${mode}" = "real" ] && [ "${NETWORK_PARTITION_REAL}" = "true" ]; then
        log_info "Removing partition (real mode): ${partition_id}"
        # Remove iptables rules (simplified - in production, track rule numbers)
        # Note: This is a simplified cleanup - production should track exact rules
        log_warn "Manual cleanup may be required for iptables rules"
        
        # Remove tc rules
        INTERFACE=$(ip route | grep default | awk '{print $5}' | head -1)
        if [ -n "${INTERFACE}" ] && command -v tc &> /dev/null; then
            tc qdisc del dev "${INTERFACE}" root 2>/dev/null || true
        fi
    else
        log_info "Removing partition (mock mode): ${partition_id}"
    fi
    
    rm -f "${partition_file}"
    log_info "Partition removed: ${partition_id}"
}

# List partitions
list_partitions() {
    log_info "Active partitions:"
    
    if [ ! -d "${PARTITION_STORAGE_DIR}" ] || [ -z "$(ls -A "${PARTITION_STORAGE_DIR}" 2>/dev/null)" ]; then
        log_info "  No active partitions"
        return
    fi
    
    for partition_file in "${PARTITION_STORAGE_DIR}"/*.json; do
        if [ -f "${partition_file}" ]; then
            local partition_id=$(basename "${partition_file}" .json)
            local type=$(jq -r '.type' "${partition_file}" 2>/dev/null || echo "unknown")
            local from=$(jq -r '.from' "${partition_file}" 2>/dev/null || echo "unknown")
            local to=$(jq -r '.to' "${partition_file}" 2>/dev/null || echo "unknown")
            local created_at=$(jq -r '.created_at' "${partition_file}" 2>/dev/null || echo "unknown")
            
            echo "  ${partition_id}: ${type} (${from} -> ${to}) created at ${created_at}"
        fi
    done
}

# Get partition status
get_partition_status() {
    local partition_id="$1"
    local partition_file="${PARTITION_STORAGE_DIR}/${partition_id}.json"
    
    if [ ! -f "${partition_file}" ]; then
        log_error "Partition not found: ${partition_id}"
        exit 1
    fi
    
    log_info "Partition status: ${partition_id}"
    cat "${partition_file}" | jq '.' 2>/dev/null || cat "${partition_file}"
}

# Simulate flapping network
simulate_flapping() {
    local from="$1"
    local to="$2"
    local interval_ms="${3:-2000}"
    local duration_ms="${4:-10000}"
    
    log_info "Simulating flapping network: ${from} -> ${to}"
    log_info "  Interval: ${interval_ms}ms"
    log_info "  Duration: ${duration_ms}ms"
    
    local start_time=$(date +%s%3N)
    local end_time=$((start_time + duration_ms))
    local partition_id=""
    
    while [ "$(date +%s%3N)" -lt "${end_time}" ]; do
        # Create partition
        partition_id=$(create_partition_mock "flapping" "${from}" "${to}" "drop")
        log_info "Partition created: ${partition_id}"
        
        sleep "$(echo "scale=3; ${interval_ms} / 1000" | bc)"
        
        # Remove partition
        if [ -n "${partition_id}" ]; then
            remove_partition "${partition_id}"
            log_info "Partition removed: ${partition_id}"
        fi
        
        sleep "$(echo "scale=3; ${interval_ms} / 1000" | bc)"
    done
    
    log_info "Flapping simulation completed"
}

# Main command handler
main() {
    local command="${1:-help}"
    
    case "${command}" in
        create)
            shift
            check_privileges "$@"
            check_network_tools
            
            local partition_type="${1:-}"
            local from="${2:-}"
            local to="${3:-}"
            shift 3 || true
            
            local action="drop"
            local delay_ms="0"
            local loss_percent="0"
            
            # Parse options
            while [ $# -gt 0 ]; do
                case "$1" in
                    --action)
                        action="$2"
                        shift 2
                        ;;
                    --delay_ms)
                        delay_ms="$2"
                        shift 2
                        ;;
                    --loss_percent)
                        loss_percent="$2"
                        shift 2
                        ;;
                    *)
                        log_error "Unknown option: $1"
                        exit 1
                        ;;
                esac
            done
            
            if [ -z "${partition_type}" ] || [ -z "${from}" ] || [ -z "${to}" ]; then
                log_error "Usage: create <partition_type> <from> <to> [--action drop|delay|reject|loss] [--delay_ms N] [--loss_percent N]"
                exit 1
            fi
            
            if [ "${NETWORK_PARTITION_REAL}" = "true" ]; then
                create_partition_real "${partition_type}" "${from}" "${to}" "${action}" "${delay_ms}" "${loss_percent}"
            else
                create_partition_mock "${partition_type}" "${from}" "${to}" "${action}"
            fi
            ;;
        remove|heal)
            shift
            local partition_id="${1:-}"
            
            if [ -z "${partition_id}" ]; then
                log_error "Usage: ${command} <partition_id>"
                exit 1
            fi
            
            remove_partition "${partition_id}"
            ;;
        list)
            list_partitions
            ;;
        status)
            shift
            local partition_id="${1:-}"
            
            if [ -z "${partition_id}" ]; then
                log_error "Usage: status <partition_id>"
                exit 1
            fi
            
            get_partition_status "${partition_id}"
            ;;
        flapping)
            shift
            local from="${1:-}"
            local to="${2:-}"
            local interval_ms="${3:-2000}"
            local duration_ms="${4:-10000}"
            
            if [ -z "${from}" ] || [ -z "${to}" ]; then
                log_error "Usage: flapping <from> <to> [interval_ms] [duration_ms]"
                exit 1
            fi
            
            simulate_flapping "${from}" "${to}" "${interval_ms}" "${duration_ms}"
            ;;
        help|*)
            cat <<EOF
R12: Network Partition Fault Injection Script

Usage:
  $0 <command> [options]

Commands:
  create <partition_type> <from> <to> [options]
    Create a network partition
    Options:
      --action drop|delay|reject|loss  (default: drop)
      --delay_ms N                     (for delay action)
      --loss_percent N                 (for loss action)
    
  remove <partition_id>
    Remove a network partition
  
  heal <partition_id>
    Alias for remove (heal partition)
  
  list
    List all active partitions
  
  status <partition_id>
    Get status of a partition
  
  flapping <from> <to> [interval_ms] [duration_ms]
    Simulate flapping network

Examples:
  # Create single-instance partition
  $0 create single_instance router nats --action drop
  
  # Create partition with delay
  $0 create single_instance router nats --action delay --delay_ms 5000
  
  # Simulate flapping network
  $0 flapping router nats 2000 10000

Environment Variables:
  NETWORK_PARTITION_REAL=false  Use mock mode (default)
  NETWORK_PARTITION_REAL=true   Use real network tools (requires root)

EOF
            ;;
    esac
}

# Run main
main "$@"

