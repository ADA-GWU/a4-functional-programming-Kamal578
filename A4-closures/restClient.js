// restClient.js

const axios = require("axios");

// The function that builds the request progressively with closures
const restClient =
  (method) =>
  (url) =>
  (endpoint) =>
  (params = {}) => {
    const fullUrl = `${url}${endpoint}`;

    // Use axios to make the HTTP request
    return axios({
      method: method,
      url: fullUrl,
      data: params, // Data is used for POST/PUT requests
    })
      .then((response) => response.data)
      .catch((error) => {
        console.error("Error:", error);
        throw error;
      });
  };

// Export the function so it can be used in other files
module.exports = restClient;
