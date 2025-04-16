import { promises as fs } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import { connect, createSigner } from '@permaweb/aoconnect/node';

/**
 * HyperBEAM API Client
 * 
 * A client for interacting with HyperBEAM nodes, supporting both
 * authenticated (signed) and unauthenticated requests.
 */
export class HyperBEAMClient {
  /**
   * Create a new HyperBEAM client
   * 
   * @param {Object} config - Configuration options
   * @param {string} config.nodeUrl - The URL of the HyperBEAM node
   * @param {string} [config.mode='mainnet'] - The network mode
   * @param {string} [config.device=''] - The device to interact with
   * @param {string|Object} [config.keyPath=null] - Path to the wallet key file or key object directly
   */
  constructor({ 
    nodeUrl = 'http://localhost:10000', 
    mode = 'mainnet', 
    device = '',
    keyPath = 'wallet.json' 
  }) {
    this.nodeUrl = nodeUrl;
    this.mode = mode;
    this.device = device;
    this.keyPath = keyPath;
    this.keyData = null;
    this.initialized = false;
  }

  /**
   * Initialize the client by loading keys if provided
   */
  async initialize() {
    if (this.initialized) return;
    
    if (this.keyPath) {
      if (typeof this.keyPath === 'string') {
        this.keyData = await this.loadKeyFromFile(this.keyPath);
      } else {
        // Assume keyPath is the key data object directly
        this.keyData = this.keyPath;
      }
    }
    
    this.initialized = true;
    return this;
  }

  /**
   * Load a key from a JSON file at the specified path
   * @param {string} filePath - Path to the JSON key file
   * @returns {Promise<object>} - The parsed key data
   * @private
   */
  async loadKeyFromFile(filePath) {
    try {
      // Handle both absolute and relative paths
      let resolvedPath = filePath;
      if (!filePath.startsWith('/')) {
        const __filename = fileURLToPath(import.meta.url);
        const __dirname = dirname(__filename);
        resolvedPath = join(__dirname, filePath);
      }
      
      const fileContent = await fs.readFile(resolvedPath, 'utf8');
      return JSON.parse(fileContent);
    } catch (error) {
      throw new Error(`Failed to load key file: ${error.message}`);
    }
  }

  /**
   * Extract and parse the response body
   * @param {Response} response - The fetch response object
   * @returns {Promise<any>} - The parsed response body
   * @private
   */
  async extractResponseBody(response) {
    const contentType = response.headers?.get?.('content-type') || '';
    
    try {
      if (contentType.includes('application/json')) {
        return await response.json();
      } else {
        const text = await response.text();
        
        if (text && (text.trim().startsWith('{') || text.trim().startsWith('['))) {
          try {
            return JSON.parse(text);
          } catch (e) {
            return text;
          }
        }
        return text;
      }
    } catch (error) {
      return `[Error parsing response: ${error.message}]`;
    }
  }

  /**
   * Format response for consistent output
   * @param {Response} response - The response from fetch or aoconnect
   * @param {any} body - The parsed body
   * @returns {object} - Formatted response
   * @private
   */
  formatResponse(response, body) {
    if (!response) {
      return { status: 'unknown', headers: {}, body };
    }
    
    return {
      status: response.status,
      headers: {
        date: response.headers?.get?.('date') || response.date,
        server: response.headers?.get?.('server') || response.server,
        'content-type': response.headers?.get?.('content-type') || response['content-type']
      },
      body
    };
  }

  /**
   * Make a GET request to fetch data from the specified path
   * 
   * @param {string} path - The path to retrieve data from
   * @param {Object} [options] - Additional request options
   * @param {boolean} [options.debug=false] - Whether to log debug information
   * @returns {Promise<object>} - The response from the server
   */
  async get(path, { debug = false } = {}) {
    await this.initialize();
    
    try {
      const fullUrl = `${this.nodeUrl}${path}`;
      if (debug) console.log(`GET ${fullUrl}`);
      
      const response = await fetch(fullUrl);
      if (debug) console.log(`Response status: ${response.status}`);
      
      const body = await this.extractResponseBody(response);
      if (debug) console.log(`Response body:`, body);
      
      return this.formatResponse(response, body);
    } catch (error) {
      console.error(`GET request failed: ${error.message}`);
      return {
        status: 'error',
        error: error.message,
        body: null
      };
    }
  }

  /**
   * Make a signed POST request to the specified path
   * 
   * @param {string} path - The path to send data to
   * @param {object} data - The data to include in the request
   * @param {Object} [options] - Additional request options
   * @param {boolean} [options.debug=false] - Whether to log debug information
   * @returns {Promise<object>} - The response from the server
   */
  async post(path, data, { debug = false } = {}) {
    await this.initialize();
    
    if (!this.keyData) {
      throw new Error('Wallet key is required for POST requests. Initialize client with keyPath.');
    }
    
    try {
      if (debug) {
        console.log(`POST ${this.nodeUrl}${path}`);
        console.log(`Request data:`, data);
      }
      
      const signer = createSigner(this.keyData);
      
      const { request } = connect({
        MODE: this.mode, 
        URL: this.nodeUrl,
        device: this.device,
        signer
      });
      
      const requestParams = {
        path,
        method: 'POST',
        ...data
      };
      
      const response = await request(requestParams);
      
      let body;
      if (response.body && typeof response.body.text === 'function') {
        const text = await response.body.text();
        try {
          body = JSON.parse(text);
        } catch (e) {
          body = text || '';
        }
      } else {
        body = response.body || '';
      }
      
      if (debug) console.log(`Response status: ${response.status}`);
      
      return this.formatResponse(response, body);
    } catch (error) {
      console.error(`POST request failed: ${error.message}`);
      return {
        status: 'error',
        error: error.message,
        body: null
      };
    }
  }
}
