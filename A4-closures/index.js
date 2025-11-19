// index.js

const restClient = require("./restClient");

// Example 1: Making a GET request
restClient("GET")("https://jsonplaceholder.typicode.com")("/posts/1")()
  .then((data) => {
    console.log("GET response:", data);
  })
  .catch((error) => {
    console.error("GET request failed:", error);
  });

// Example 2: Making a POST request
restClient("POST")("https://jsonplaceholder.typicode.com")("/posts")({
  title: "ASP Assignment",
  body: "Functional Programming (Closures)",
  userId: 1,
})
  .then((data) => {
    console.log("POST response:", data);
  })
  .catch((error) => {
    console.error("POST request failed:", error);
  });
