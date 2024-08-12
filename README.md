# simulation_ocaml

![console-screenshot](https://github.com/user-attachments/assets/29486be3-3f7a-4a7e-9283-46a7de6fb9fb)

The goal of the project is a step-by-step simulation of a 2d world populated by herbivores and predators

Created according to the technical specifications presented in [this course](https://zhukovsd.github.io/java-backend-learning-course/projects/simulation/)
## Run Locally

> [!IMPORTANT]  
> [OCaml](https://ocaml.org/install) and [Dune](https://dune.build/install) should be installed to run this project

Clone the project

```bash
git clone git@github.com:krios2146/simulation_ocaml.git
```

Go to the project directory

```bash
cd simulation_ocaml
```

Run the simulation with

```bash
dune build && dune exec simulation
```
## Considerations


This is by no means production-level OCaml code

I have very limited experience with functional programming languages. I have only used Gleam before and a little bit of Rust. The project was written just for fun in my spare time

---

The [technical specification](https://zhukovsd.github.io/java-backend-learning-course/projects/simulation/) was created for Java and heavily relies on the OOP features of the language

I'm aware that OCaml has OOP features, but I didn't use them intentionally—I wanted to try a functional programming approach

Instead of using classes to manage state, I created state using the OCaml type system. Functions are the behavior of the state

---

Movement of the creatures on the map is bugged. For some reason, herbivores stop after a certain number of iterations. I don't plan to debug this, so just leave it as is

Pause, resume, and step-by-step execution are not implemented
