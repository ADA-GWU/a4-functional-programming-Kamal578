# A4-currying

Functional programming assignment that builds a curried REST client in Haskell. Each stage of the client captures one part of the request (HTTP method → base URL → endpoint) and the final call performs the HTTP side effect in isolation.

## Functional Design
- `restClient` lives in `src/Lib.hs` as three nested functions: `restClient "GET" "https://jsonplaceholder.typicode.com" "/posts/1"` progressively captures the method, host, and endpoint before issuing the request. `app/Main.hs` imports it and stays responsible only for performing the final IO action and printing the response.
- The composed action uses `http-conduit` to create a TLS-enabled `Manager`, parse the full URL, override the HTTP method, and finally execute the request with `httpLbs`.
- Optional parameters can be provided by including them in the endpoint string, e.g. `"/posts?userId=1"`; the curried structure keeps this logic in the pure portion of the code until the final IO action runs.
- `Main.hs` stays a thin executable that imports the curried builder and performs the IO action while printing the response.

## Prerequisites

#### macOS
1. Install [Homebrew](https://brew.sh/) if not already installed.
2. Install Haskell Stack: `brew install haskell-stack`.
3. Ensure internet access for Stack to download GHC and for the JSONPlaceholder demo API.

#### Windows
1. Download and install Haskell Stack from [haskellstack.org](https://haskellstack.org).
2. Ensure internet access for Stack to download GHC and for the JSONPlaceholder demo API.

#### Linux
1. Update your package list:
  ```bash
  sudo apt update
  ```
2. Install Haskell Stack:
  ```bash
  sudo apt install haskell-stack
  ```
  Alternatively, download the latest version from [haskellstack.org](https://haskellstack.org) and follow the installation instructions.
3. Verify the installation:
  ```bash
  stack --version
  ```
4. Ensure internet access for Stack to download GHC and for the JSONPlaceholder demo API.

## Build & Run

First, ensure there is GHC and dependencies installed via Stack. You can run this command in the project root:
```bash
stack ghc -- --version                    # verifies GHC is installed
stack setup                             # installs GHC if not already present
```

Then, build and run the curried REST client:

```bash
# From the project root (A4-currying)
stack build                   # downloads GHC (first run) and compiles dependencies
stack exec A4-currying-exe    # executes the curried REST request
```

Run the integration tests (hits JSONPlaceholder, so requires an internet connection):

```bash
stack test
```

First execution prints the JSON response for `GET https://jsonplaceholder.typicode.com/posts/1`, e.g.

```json
{
  "userId": 1,
  "id": 1,
  "title": "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
  "body": "quia et suscipit\nsuscipit recusandae..."
}
```

## Customizing Requests
- **HTTP method** – change the first argument, e.g. `"POST"`, `"PUT"`, `"DELETE"`.
- **Base URL** – second argument; switch to any API host such as `"https://api.github.com"`.
- **Endpoint & query parameters** – third argument; include path and optional query string, such as `"/users?name=Samir"` to pass parameters.

Because every argument returns a new partially-applied function, you can compose and reuse them:

```haskell
let jsonPlaceholder = restClient "GET" "https://jsonplaceholder.typicode.com"
response <- jsonPlaceholder "/comments?postId=1"
```

This pattern mirrors currying/closures in other languages by keeping side effects at the outermost layer while the inner layers remain pure.

## Repository Layout
- `src/Lib.hs` – curried REST builder that exposes `restClient`.
- `app/Main.hs` – imports `restClient` and demonstrates issuing a request.
- `test/Spec.hs` – Hspec tests verifying `/posts/1` and `/posts/2` responses.
- `package.yaml` – Stack project definition plus `http-conduit`, `bytestring`, `text` dependencies.
- `stack.yaml` / `stack.yaml.lock` – pinned resolver information for reproducible builds.