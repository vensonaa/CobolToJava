#!/bin/bash

echo "🛑 Stopping COBOL to Java Conversion System..."

# Kill processes by port
echo "Stopping frontend (port 3000)..."
pkill -f "react-scripts start" 2>/dev/null

echo "Stopping backend (port 8000)..."
pkill -f "python main.py" 2>/dev/null

echo "Stopping MCP server (port 8001)..."
pkill -f "python server.py" 2>/dev/null

echo "✅ All services stopped!"
