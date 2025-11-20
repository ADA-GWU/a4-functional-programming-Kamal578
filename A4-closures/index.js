// index.js

const restClient = require("./restClient");

const jsonPlaceholder = "https://jsonplaceholder.typicode.com";

const postPayload = {
  title: "ASP Assignment",
  body: "Functional Programming (Closures)",
  userId: 1,
};

const putPayload = {
  id: 1,
  title: "ASP Assignment - Updated",
  body: "Functional Programming (Closures) Updated",
  userId: 1,
};

async function run() {
  try {
    console.log("GET /posts/1 response:");
    const getData = await restClient("GET")(jsonPlaceholder)("/posts/1")();
    console.log(getData);

    console.log("\nPOST /posts response:");
    const postData = await restClient("POST")(jsonPlaceholder)("/posts")(postPayload);
    console.log(postData);

    console.log("\nPUT /posts/1 response:");
    const putData = await restClient("PUT")(jsonPlaceholder)("/posts/1")(putPayload);
    console.log(putData);

    console.log("\nDELETE /posts/1 response:");
    const deleteData = await restClient("DELETE")(jsonPlaceholder)("/posts/1")();
    console.log(deleteData);
  } catch (error) {
    console.error("REST client demo failed:", error);
  }
}

run();
