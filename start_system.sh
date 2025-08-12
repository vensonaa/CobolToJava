#!/bin/bash

# COBOL to Java Conversion System Startup Script

echo "🚀 Starting COBOL to Java Conversion System..."

# Function to check if a port is in use
check_port() {
    if lsof -Pi :$1 -sTCP:LISTEN -t >/dev/null ; then
        echo "Port $1 is already in use"
        return 1
    else
        return 0
    fi
}

# Start MCP server
echo "Starting MCP server..."
cd mcp_server
source venv/bin/activate
python server.py &
MCP_PID=$!
cd ..

# Wait for MCP server to start
sleep 3

# Start backend
echo "Starting backend server..."
cd backend
source venv/bin/activate
python main.py &
BACKEND_PID=$!
cd ..

# Wait for backend to start
sleep 3

# Start frontend
echo "Starting frontend..."
cd frontend
npm start &
FRONTEND_PID=$!
cd ..

echo "✅ All services started!"
echo "Frontend: http://localhost:3000"
echo "Backend: http://localhost:8000"
echo "MCP Server: http://localhost:8001"

# Function to cleanup on exit
cleanup() {
    echo "Shutting down services..."
    kill $FRONTEND_PID 2>/dev/null
    kill $BACKEND_PID 2>/dev/null
    kill $MCP_PID 2>/dev/null
    exit 0
}

# Set up signal handlers
trap cleanup SIGINT SIGTERM

# Wait for all processes
wait
