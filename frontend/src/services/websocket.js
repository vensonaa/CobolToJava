class WebSocketService {
  constructor() {
    this.ws = null;
    this.url = null;
    this.clientId = null;
    this.isConnected = false;
    this.reconnectAttempts = 0;
    this.maxReconnectAttempts = 5;
    this.reconnectDelay = 1000;
    this.messageHandlers = [];
    this.connectHandlers = [];
    this.disconnectHandlers = [];
    this.errorHandlers = [];
  }

  connect(clientId = null) {
    if (this.isConnected) {
      console.log('WebSocket already connected');
      return;
    }

    this.clientId = clientId || this.generateClientId();
    this.url = this.getWebSocketUrl();

    try {
      this.ws = new WebSocket(this.url);
      this.setupEventHandlers();
    } catch (error) {
      console.error('Failed to create WebSocket connection:', error);
      this.handleError(error);
    }
  }

  disconnect() {
    if (this.ws) {
      this.ws.close();
      this.ws = null;
    }
    this.isConnected = false;
    this.reconnectAttempts = 0;
  }

  setupEventHandlers() {
    this.ws.onopen = () => {
      console.log('WebSocket connected');
      this.isConnected = true;
      this.reconnectAttempts = 0;
      this.notifyConnectHandlers();
    };

    this.ws.onmessage = (event) => {
      try {
        const data = JSON.parse(event.data);
        this.notifyMessageHandlers(data);
      } catch (error) {
        console.error('Error parsing WebSocket message:', error);
        this.handleError(error);
      }
    };

    this.ws.onclose = (event) => {
      console.log('WebSocket disconnected:', event.code, event.reason);
      this.isConnected = false;
      this.notifyDisconnectHandlers(event);
      
      // Attempt to reconnect if not manually closed
      if (event.code !== 1000 && this.reconnectAttempts < this.maxReconnectAttempts) {
        this.attemptReconnect();
      }
    };

    this.ws.onerror = (error) => {
      console.error('WebSocket error:', error);
      this.handleError(error);
    };
  }

  attemptReconnect() {
    this.reconnectAttempts++;
    const delay = this.reconnectDelay * Math.pow(2, this.reconnectAttempts - 1);
    
    console.log(`Attempting to reconnect in ${delay}ms (attempt ${this.reconnectAttempts}/${this.maxReconnectAttempts})`);
    
    setTimeout(() => {
      if (!this.isConnected) {
        this.connect(this.clientId);
      }
    }, delay);
  }

  send(message) {
    if (!this.isConnected || !this.ws) {
      console.error('WebSocket not connected');
      return false;
    }

    try {
      const messageStr = typeof message === 'string' ? message : JSON.stringify(message);
      this.ws.send(messageStr);
      return true;
    } catch (error) {
      console.error('Error sending WebSocket message:', error);
      this.handleError(error);
      return false;
    }
  }

  // Event handler registration
  onMessage(handler) {
    this.messageHandlers.push(handler);
    return () => {
      const index = this.messageHandlers.indexOf(handler);
      if (index > -1) {
        this.messageHandlers.splice(index, 1);
      }
    };
  }

  onConnect(handler) {
    this.connectHandlers.push(handler);
    return () => {
      const index = this.connectHandlers.indexOf(handler);
      if (index > -1) {
        this.connectHandlers.splice(index, 1);
      }
    };
  }

  onDisconnect(handler) {
    this.disconnectHandlers.push(handler);
    return () => {
      const index = this.disconnectHandlers.indexOf(handler);
      if (index > -1) {
        this.disconnectHandlers.splice(index, 1);
      }
    };
  }

  onError(handler) {
    this.errorHandlers.push(handler);
    return () => {
      const index = this.errorHandlers.indexOf(handler);
      if (index > -1) {
        this.errorHandlers.splice(index, 1);
      }
    };
  }

  // Event notification
  notifyMessageHandlers(data) {
    this.messageHandlers.forEach(handler => {
      try {
        handler(data);
      } catch (error) {
        console.error('Error in message handler:', error);
      }
    });
  }

  notifyConnectHandlers() {
    this.connectHandlers.forEach(handler => {
      try {
        handler();
      } catch (error) {
        console.error('Error in connect handler:', error);
      }
    });
  }

  notifyDisconnectHandlers(event) {
    this.disconnectHandlers.forEach(handler => {
      try {
        handler(event);
      } catch (error) {
        console.error('Error in disconnect handler:', error);
      }
    });
  }

  handleError(error) {
    this.errorHandlers.forEach(handler => {
      try {
        handler(error);
      } catch (handlerError) {
        console.error('Error in error handler:', handlerError);
      }
    });
  }

  // Utility methods
  generateClientId() {
    return 'client_' + Math.random().toString(36).substr(2, 9) + '_' + Date.now();
  }

  getWebSocketUrl() {
    const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
    const host = process.env.REACT_APP_WS_HOST || window.location.host;
    return `${protocol}//${host}/ws/${this.clientId}`;
  }

  // Connection status
  getConnectionStatus() {
    return {
      isConnected: this.isConnected,
      clientId: this.clientId,
      reconnectAttempts: this.reconnectAttempts,
      maxReconnectAttempts: this.maxReconnectAttempts
    };
  }

  // Message types for conversion system
  sendConversionRequest(request) {
    return this.send({
      type: 'conversion_request',
      data: request
    });
  }

  sendFeedback(feedback) {
    return this.send({
      type: 'feedback',
      data: feedback
    });
  }

  sendHeartbeat() {
    return this.send({
      type: 'heartbeat',
      timestamp: Date.now()
    });
  }

  // Configuration
  setReconnectConfig(maxAttempts, delay) {
    this.maxReconnectAttempts = maxAttempts;
    this.reconnectDelay = delay;
  }

  // Cleanup
  cleanup() {
    this.disconnect();
    this.messageHandlers = [];
    this.connectHandlers = [];
    this.disconnectHandlers = [];
    this.errorHandlers = [];
  }
}

// Create a singleton instance
const webSocketService = new WebSocketService();

export { WebSocketService, webSocketService as default };
