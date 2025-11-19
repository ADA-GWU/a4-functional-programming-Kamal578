## A4-closures (Node.js)

Functional version of the REST request builder implemented with JavaScript closures.  
Each call captures the HTTP method → base URL → endpoint and returns a function that performs the actual Axios request when invoked. This mirrors the Haskell currying assignment but in the Node.js runtime.

### Project Layout
- `restClient.js` – curried/closure-based builder that wraps Axios.
- `index.js` – demo script showing both `GET /posts/1` and `POST /posts` calls against JSONPlaceholder.
- `package.json` / `package-lock.json` – Node dependencies (`axios`) and scripts.

### Prerequisites
- Node.js 18+ (or any version compatible with Axios 1.x).
- npm (bundled with Node).

### Install Dependencies
```bash
npm install
```

### Run the Demo
```bash
node index.js
```

You should see console output for:
- GET request returning the `/posts/1` JSON object.
- POST request that sends `{ title: "ASP Assignment", body: "Functional Programming (Closures)", userId: 1 }` and prints the mocked JSONPlaceholder response.

### How It Works
```javascript
const restClient =
  (method) =>
  (url) =>
  (endpoint) =>
  (params = {}) => axios({ method, url: `${url}${endpoint}`, data: params });
```
Each level returns a new function that closes over the previous arguments. The final function optionally takes a payload (`params`) so POST/PUT requests can supply a body, while GET calls simply invoke it with no arguments.
