#!/bin/bash

# COBOL to Java Conversion System Setup Script
echo "🚀 Setting up COBOL to Java Conversion System..."

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if Python is installed
check_python() {
    print_status "Checking Python installation..."
    if command -v python3 &> /dev/null; then
        PYTHON_VERSION=$(python3 --version)
        print_success "Python found: $PYTHON_VERSION"
    else
        print_error "Python 3 is not installed. Please install Python 3.8+ first."
        exit 1
    fi
}

# Check if Node.js is installed
check_node() {
    print_status "Checking Node.js installation..."
    if command -v node &> /dev/null; then
        NODE_VERSION=$(node --version)
        print_success "Node.js found: $NODE_VERSION"
    else
        print_error "Node.js is not installed. Please install Node.js 16+ first."
        exit 1
    fi
}

# Check if npm is installed
check_npm() {
    print_status "Checking npm installation..."
    if command -v npm &> /dev/null; then
        NPM_VERSION=$(npm --version)
        print_success "npm found: $NPM_VERSION"
    else
        print_error "npm is not installed. Please install npm first."
        exit 1
    fi
}

# Setup backend
setup_backend() {
    print_status "Setting up backend..."
    cd backend
    
    # Create virtual environment
    print_status "Creating Python virtual environment..."
    python3 -m venv venv
    
    # Activate virtual environment
    source venv/bin/activate
    
    # Upgrade pip
    print_status "Upgrading pip..."
    pip install --upgrade pip
    
    # Install requirements
    print_status "Installing Python dependencies..."
    pip install -r requirements.txt
    
    # Create .env file if it doesn't exist
    if [ ! -f .env ]; then
        print_status "Creating .env file..."
        cp ../env.example .env
        print_warning "Please edit .env file with your API keys before running the application."
    fi
    
    cd ..
    print_success "Backend setup completed!"
}

# Setup MCP server
setup_mcp_server() {
    print_status "Setting up MCP server..."
    cd mcp_server
    
    # Create virtual environment
    print_status "Creating Python virtual environment for MCP server..."
    python3 -m venv venv
    
    # Activate virtual environment
    source venv/bin/activate
    
    # Upgrade pip
    print_status "Upgrading pip..."
    pip install --upgrade pip
    
    # Install requirements
    print_status "Installing MCP server dependencies..."
    pip install -r requirements.txt
    
    # Create .env file if it doesn't exist
    if [ ! -f .env ]; then
        print_status "Creating .env file for MCP server..."
        cp ../env.example .env
    fi
    
    cd ..
    print_success "MCP server setup completed!"
}

# Setup frontend
setup_frontend() {
    print_status "Setting up frontend..."
    cd frontend
    
    # Install dependencies
    print_status "Installing Node.js dependencies..."
    npm install
    
    # Create .env file if it doesn't exist
    if [ ! -f .env ]; then
        print_status "Creating .env file for frontend..."
        cp ../env.example .env
    fi
    
    cd ..
    print_success "Frontend setup completed!"
}

# Create sample COBOL file
create_sample_files() {
    print_status "Creating sample files..."
    
    # Create sample COBOL file
    cat > sample_cobol.cbl << 'EOF'
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01  WS-NAME     PIC X(20).
           01  WS-COUNTER  PIC 9(3).
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Enter your name: ".
           ACCEPT WS-NAME.
           
           PERFORM VARYING WS-COUNTER FROM 1 BY 1
               UNTIL WS-COUNTER > 3
               DISPLAY "Hello, " WS-NAME
           END-PERFORM.
           
           IF WS-COUNTER > 2
               DISPLAY "Counter is greater than 2"
           ELSE
               DISPLAY "Counter is 2 or less"
           END-IF.
           
           STOP RUN.
EOF
    
    print_success "Sample COBOL file created: sample_cobol.cbl"
}

# Create startup script
create_startup_script() {
    print_status "Creating startup script..."
    
    cat > start_system.sh << 'EOF'
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
EOF
    
    chmod +x start_system.sh
    print_success "Startup script created: start_system.sh"
}

# Create stop script
create_stop_script() {
    print_status "Creating stop script..."
    
    cat > stop_system.sh << 'EOF'
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
EOF
    
    chmod +x stop_system.sh
    print_success "Stop script created: stop_system.sh"
}

# Main setup function
main() {
    echo "=========================================="
    echo "COBOL to Java Conversion System Setup"
    echo "=========================================="
    
    # Check prerequisites
    check_python
    check_node
    check_npm
    
    # Setup components
    setup_backend
    setup_mcp_server
    setup_frontend
    
    # Create additional files
    create_sample_files
    create_startup_script
    create_stop_script
    
    echo ""
    echo "=========================================="
    print_success "Setup completed successfully!"
    echo "=========================================="
    echo ""
    echo "Next steps:"
    echo "1. Edit the .env files in backend/ and mcp_server/ with your API keys"
    echo "2. Run './start_system.sh' to start all services"
    echo "3. Open http://localhost:3000 in your browser"
    echo "4. Use './stop_system.sh' to stop all services"
    echo ""
    echo "Sample COBOL file: sample_cobol.cbl"
    echo ""
}

# Run main function
main
