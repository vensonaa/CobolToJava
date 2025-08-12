import axios from 'axios';

const API_BASE_URL = process.env.REACT_APP_API_URL || 'http://localhost:8000';

// Create axios instance with default config
const apiClient = axios.create({
  baseURL: API_BASE_URL,
  timeout: 30000,
  headers: {
    'Content-Type': 'application/json',
  },
});

// Request interceptor
apiClient.interceptors.request.use(
  (config) => {
    // Add any auth tokens here if needed
    return config;
  },
  (error) => {
    return Promise.reject(error);
  }
);

// Response interceptor
apiClient.interceptors.response.use(
  (response) => {
    return response.data;
  },
  (error) => {
    console.error('API Error:', error);
    if (error.response) {
      // Server responded with error status
      throw new Error(error.response.data.detail || error.response.data.message || 'API Error');
    } else if (error.request) {
      // Request was made but no response received
      throw new Error('No response from server. Please check your connection.');
    } else {
      // Something else happened
      throw new Error('Request failed. Please try again.');
    }
  }
);

export const ApiService = {
  // Conversion endpoints
  async startConversion(request) {
    return await apiClient.post('/convert', request);
  },

  async getConversionStatus(jobId) {
    return await apiClient.get(`/status/${jobId}`);
  },

  async submitFeedback(feedback) {
    return await apiClient.post('/feedback', feedback);
  },

  async getConversionHistory(limit = 50, offset = 0) {
    return await apiClient.get('/history', {
      params: { limit, offset }
    });
  },

  async startBatchConversion(requests) {
    return await apiClient.post('/batch', requests);
  },

  // Health check
  async healthCheck() {
    return await apiClient.get('/health');
  },

  // MCP server endpoints
  async callMCPMethod(method, params) {
    return await apiClient.post('/mcp/call', {
      method,
      params
    });
  },

  async streamMCPAnalysis(code, analysisType = 'comprehensive') {
    return await apiClient.post('/mcp/stream', {
      method: 'stream_analysis',
      params: {
        code,
        analysis_type: analysisType
      }
    });
  },

  // File operations
  async uploadFile(file) {
    const formData = new FormData();
    formData.append('file', file);
    
    return await apiClient.post('/upload', formData, {
      headers: {
        'Content-Type': 'multipart/form-data',
      },
    });
  },

  async downloadResult(jobId) {
    return await apiClient.get(`/download/${jobId}`, {
      responseType: 'blob'
    });
  },

  // Utility methods
  async validateCobolCode(code) {
    return await this.callMCPMethod('analyze_cobol', {
      code,
      analysis_type: 'validation'
    });
  },

  async getConversionSuggestions(code) {
    return await this.callMCPMethod('get_conversion_suggestions', {
      cobol_code: code
    });
  },

  async estimateComplexity(code) {
    return await this.callMCPMethod('estimate_complexity', {
      cobol_code: code
    });
  },

  async compileJavaCode(code) {
    return await this.callMCPMethod('compile_java', {
      java_code: code
    });
  },

  async validateConversion(cobolCode, javaCode) {
    return await this.callMCPMethod('validate_conversion', {
      cobol_code: cobolCode,
      java_code: javaCode
    });
  },

  async generateDocumentation(cobolCode, javaCode) {
    return await this.callMCPMethod('generate_documentation', {
      cobol_code: cobolCode,
      java_code: javaCode
    });
  },

  async getBestPractices(context) {
    return await this.callMCPMethod('get_best_practices', {
      context
    });
  },

  // WebSocket connection helper
  getWebSocketUrl(clientId) {
    return `${API_BASE_URL.replace('http', 'ws')}/ws/${clientId}`;
  },

  // Error handling
  handleError(error) {
    console.error('API Service Error:', error);
    
    if (error.message.includes('Network Error')) {
      return {
        type: 'network',
        message: 'Unable to connect to server. Please check your internet connection.'
      };
    }
    
    if (error.message.includes('timeout')) {
      return {
        type: 'timeout',
        message: 'Request timed out. Please try again.'
      };
    }
    
    return {
      type: 'api',
      message: error.message || 'An unexpected error occurred.'
    };
  },

  // Retry logic for failed requests
  async retryRequest(requestFn, maxRetries = 3, delay = 1000) {
    let lastError;
    
    for (let attempt = 1; attempt <= maxRetries; attempt++) {
      try {
        return await requestFn();
      } catch (error) {
        lastError = error;
        
        if (attempt === maxRetries) {
          throw error;
        }
        
        // Wait before retrying
        await new Promise(resolve => setTimeout(resolve, delay * attempt));
      }
    }
    
    throw lastError;
  },

  // Batch operations
  async processBatchConversions(conversions, onProgress) {
    const results = [];
    const total = conversions.length;
    
    for (let i = 0; i < total; i++) {
      try {
        const result = await this.startConversion(conversions[i]);
        results.push({
          index: i,
          success: true,
          data: result
        });
      } catch (error) {
        results.push({
          index: i,
          success: false,
          error: error.message
        });
      }
      
      // Report progress
      if (onProgress) {
        onProgress({
          completed: i + 1,
          total,
          percentage: ((i + 1) / total) * 100,
          current: conversions[i]
        });
      }
    }
    
    return results;
  },

  // Cache management
  cache: new Map(),

  async getCachedOrFetch(key, fetchFn, ttl = 5 * 60 * 1000) { // 5 minutes default
    const cached = this.cache.get(key);
    
    if (cached && Date.now() - cached.timestamp < ttl) {
      return cached.data;
    }
    
    const data = await fetchFn();
    this.cache.set(key, {
      data,
      timestamp: Date.now()
    });
    
    return data;
  },

  clearCache() {
    this.cache.clear();
  },

  // Real-time status polling
  async pollConversionStatus(jobId, onUpdate, interval = 2000, maxAttempts = 150) { // 5 minutes max
    let attempts = 0;
    
    const poll = async () => {
      try {
        const status = await this.getConversionStatus(jobId);
        
        if (onUpdate) {
          onUpdate(status);
        }
        
        // Stop polling if conversion is complete or failed
        if (['completed', 'failed', 'needs_feedback'].includes(status.status)) {
          return status;
        }
        
        attempts++;
        if (attempts >= maxAttempts) {
          throw new Error('Polling timeout - conversion taking too long');
        }
        
        // Continue polling
        setTimeout(poll, interval);
        
      } catch (error) {
        console.error('Polling error:', error);
        throw error;
      }
    };
    
    return poll();
  }
};

export default ApiService;
