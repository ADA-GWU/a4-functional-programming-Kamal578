[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/31ycnkRP)

# **Assignment 4. Functional Programming**

### **Name:** Kamal Ahmadov

### **Program:** MSc. Computer Science & Data Analytics

### **E-mails:** [kahmadov24700@ada.edu.az](mailto:kahmadov24700@ada.edu.az); [kamal.ahmadov@gwu.edu](mailto:kamal.ahmadov@gwu.edu)

### **Course:** Fall 2025 Advanced Software Paradigms (CSCI-6221 - 10115)

---

### Functional Objective
Instead of invoking `call_rest("POST", "https://www.example.com", "users/")` imperatively, we construct a functional call chain:

```text
rest_client("POST")("https://www.example.com")("users/")()
```

Each step returns another function with the prior context baked in (method → host → path). The final invocation performs the HTTP action and can accept optional parameters (e.g., `id=123`, `name="Samir"`) that get serialized into the request.

### Repository Layout
- `A4-currying/` – Haskell Stack project showcasing currying-based request composition.  
  Includes a curried `restClient`/`restClientWithBody` builder and JSONPlaceholder integration tests.
- `A4-closures/` – Node.js version using closures to achieve the same progressive request building with Axios.
- `README.md` (this file) – assignment overview and submission notes.

Execution instructions, dependencies, and test commands live inside each respective folder’s README. Follow those guides to install prerequisites, run the demo clients, and execute tests.

### Learning Outcomes
- Practice functional composition to isolate side effects and model REST calls declaratively.
- Compare currying (Haskell) vs. closures (Node.js) for building progressive APIs.
- Reinforce the idea of capturing configuration early and performing IO only at the boundary.