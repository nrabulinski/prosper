<p align="center">
	<img src="logo.png" alt="prosper logo" height="200" width="200">
	<h1 align="center">Prosper programming language</h1>
</p>

Prosper is a programming language that eventually will aim to be a decent starting point as a system programming language.  
It focuses on simplicity while still offering modern features and tooling.  
The language is heavily inspired by Rust, C and Haskell.


## Why rewrite?

I decided to start from the very beginning because I initially made some very poor decisions and overscoped what I could achieve in a reasonable time frame.  
This time, while still the code is far from perfect or even remotely decent, 
I'm focusing on incremental improvements and starting with a working MVP rather than a big broken mess.


## End goal

The Rust codebase will be deprecated as soon as the language is able to self-host. For easy bootstrapping I want to eventually introduce a C-generating backend.  
After the compiler succesfully compiles itself, runs okay on modern hardware and the code is maintainable I will consider the project to be done and move on. 
Unless it somehow gains any real traction.
