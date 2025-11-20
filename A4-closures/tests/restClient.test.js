const test = require("node:test");
const assert = require("node:assert/strict");

const restClient = require("../restClient");

const baseUrl = "https://jsonplaceholder.typicode.com";

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

test("GET /posts/1", async () => {
  const data = await restClient("GET")(baseUrl)("/posts/1")();
  assert.equal(data.id, 1);
  assert.equal(data.userId, 1);
});

test("POST /posts", async () => {
  const data = await restClient("POST")(baseUrl)("/posts")(postPayload);
  assert.equal(data.title, postPayload.title);
  assert.equal(data.body, postPayload.body);
  assert.equal(data.userId, postPayload.userId);
  assert.equal(data.id, 101);
});

test("PUT /posts/1", async () => {
  const data = await restClient("PUT")(baseUrl)("/posts/1")(putPayload);
  assert.equal(data.id, 1);
  assert.equal(data.title, putPayload.title);
  assert.equal(data.body, putPayload.body);
  assert.equal(data.userId, putPayload.userId);
});

test("DELETE /posts/1", async () => {
  const data = await restClient("DELETE")(baseUrl)("/posts/1")();
  assert.deepEqual(data, {});
});
