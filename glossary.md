# NeuroScope Glossary

A comprehensive glossary of terms used throughout the NeuroScope mechanistic interpretability framework.

## A

**Ablation** - The process of removing or zeroing out specific model components (attention heads, neurons, layers) to study their causal contribution to model behavior.

**Activation** - The numerical output values produced by neurons or layers during a forward pass through the neural network.

**ActivationHook** - A type of hook that monitors and potentially modifies activations as they flow through the model during forward passes.

**ActivationTensor** - The fundamental data structure in NeuroScope that stores multi-dimensional activation data along with metadata about shape, device placement, and gradient requirements.

**Attention Head** - Individual components within attention layers that focus on different types of relationships between tokens. Each transformer layer typically contains multiple attention heads.

**Attention Patterns** - The matrices of weights produced by attention heads that show which tokens the model considers relevant when processing each position.

**Attention Weights** - The normalized probability distributions that determine how much each token "pays attention to" every other token in the sequence.

**AttentionAnalyzer** - A specialized analysis tool for studying attention patterns and head behaviors in transformer models.

**AttentionLayer** - A transformer layer that implements the multi-head attention mechanism, allowing the model to focus on different aspects of the input sequence simultaneously.

**AttentionVisualizer** - A component that renders attention patterns for interactive exploration and analysis.

**Attention** - The core mechanism in transformer models that allows the model to focus on different parts of the input sequence when processing each token. Mathematically computed as a weighted sum of values, where weights are determined by the similarity between queries and keys.

## B

**Batch Processing** - Processing multiple text sequences simultaneously for improved computational efficiency.

**Beam Search** - A text generation method that maintains multiple candidate sequences and selects the most probable continuation.

**Browser Integration** - NeuroScope's capability to run entirely in web browsers through SqueakJS, leveraging browser APIs for computation and visualization.

**BrowserStorage** - A component that manages data persistence using browser storage mechanisms like IndexedDB and localStorage.

## C

**CachingHook** - A type of hook that stores activations or analysis results for later retrieval and reuse.

**Canvas API** - Browser graphics API used by NeuroScope for 2D visualizations and rendering.

**CanvasRenderer** - A component that handles 2D graphics operations using the browser's Canvas API.

**Causal Intervention** - Modifying specific model components and observing how this affects outputs to establish causal relationships.

**Circuit** - A specific computational pathway through layers, attention heads, and neurons that implements a particular function or behavior.

**CircuitFinder** - A tool for automated identification of computational circuits within transformer models.

**Compositional Understanding** - The principle that complex behaviors emerge from the composition of simpler circuits and components.

**Configuration** - Model hyperparameters and architectural settings stored in a dictionary format.

## D

**DataBridge** - A utility component that facilitates efficient data transfer between Smalltalk and JavaScript environments.

**Device Placement** - The specification of where tensor data resides ('cpu', 'webgl', 'javascript') for computational optimization.

**Distributed Analysis** - The capability to spread analysis computations across multiple processes or workers for performance.

**Dropout** - A regularization technique that randomly sets a fraction of input units to zero during training to prevent overfitting. During inference, dropout is typically disabled, but some implementations may still apply it, contributing to non-deterministic outputs even with identical inputs.

## E

**EmbeddingLayer** - The transformer layer that converts token IDs into dense vector representations and adds positional information.

**Event-Driven Architecture** - NeuroScope's design pattern that responds to both user interactions and model state changes through event handlers.

## F

**Forward Pass** - The process of computing model outputs by passing input tokens through all layers sequentially.

**Fuzzy Matching** - A search technique used in file discovery that matches partial or approximate patterns.

## G

**GPU Acceleration** - Using graphics processing units (GPUs) through WebGL for faster tensor computations and visualizations.

**Gradient** - The mathematical derivative that indicates the direction and magnitude of change in a function. In neural networks, gradients are used to update model parameters during training and are tracked for certain tensors in NeuroScope for probe training and analysis.

**Gradient Tracking** - The process of maintaining gradient information for tensors to enable backpropagation and probe training.

## H

**Head Dimension** - The size of the vector space for each attention head, typically hidden_size / num_heads.

**Hidden Size** - The dimensionality of the model's internal representations, a key architectural parameter.

**Hook** - An object that intercepts and potentially modifies model execution at specific points for analysis or intervention.

**HookManager** - The central orchestrator that manages the lifecycle, execution, and coordination of all hooks attached to a model.

**HuggingFace Hub** - A repository of pre-trained models that NeuroScope can load and analyze.

## I

**IndexedDB** - Browser storage mechanism used for persisting large activation datasets.

**Inference** - Running a model to generate predictions without updating model parameters.

**Interactive Exploration** - Real-time manipulation and visualization of model components through direct user interaction.

**InteractiveLens** - The primary GUI component for real-time, interactive exploration of transformer model internals.

**Intervention** - The act of modifying model activations or components during execution to study their effects.

**InterventionHook** - A type of hook that modifies activations during forward passes to test causal relationships.

## J

**JavaScript Bridge** - The interface that enables direct integration between Smalltalk and JavaScript for accessing web ML libraries.

**JSInterface** - A component that provides JavaScript interoperability for browser-based operations.

## K

**Key-Value-Query** - The three components of attention computation: keys and queries determine attention weights, values are aggregated.

## L

**Layer Normalization** - A normalization technique applied within transformer layers to stabilize training and improve performance.

**Layer** - A computational unit in a neural network that transforms input data through learned parameters. In transformers, layers typically consist of attention mechanisms and feed-forward networks (MLPs) that process information sequentially.

**Linear Probe** - A simple linear classifier trained on internal model representations to understand what information is encoded.

**Logits** - The raw, unnormalized output scores from the model before applying softmax to get probabilities.

## M

**Mechanistic Interpretability** - A research field focused on understanding the internal mechanisms of neural networks by reverse-engineering their computational processes.

**Message-Passing Paradigm** - Smalltalk's object-oriented communication pattern where objects interact by sending messages.

**MLPLayer** - The Multi-Layer Perceptron component of transformer layers that applies non-linear transformations.

**Model** - A neural network system, specifically a transformer architecture in NeuroScope's context, that has been trained to perform language tasks. The model consists of multiple layers, parameters, and computational components that work together to process and generate text.

**Model Configuration** - A dictionary containing architectural parameters like layer count, hidden size, and attention heads.

**Multi-Head Attention** - The attention mechanism that uses multiple parallel attention heads to capture different types of relationships.

## N

**Neuron** - An individual computational unit within MLP layers that can be analyzed for its activation patterns and function.

**NeuronAnalyzer** - A tool for studying individual neuron behaviors and their contributions to model processing.

**Next Token Prediction** - The fundamental task of language models: predicting the most likely next token given a sequence.

## O

**Object-Oriented Analysis** - NeuroScope's approach where activations, hooks, and interventions are all first-class objects that can be inspected and manipulated.

## P

**Positional Embeddings** - Vector representations that encode the position of tokens within a sequence.

**ProbeHook** - A type of hook specifically designed for training and evaluating linear probes on model activations.

**Probe Training** - The process of training simple classifiers on internal representations to understand encoded information.

## Q

**Query-Key-Value** - The three matrices used in attention computation, derived from input representations through learned linear transformations.

## R

**Real-Time Visualization** - Live updates of model internals as computation proceeds, providing immediate feedback.

**Residual Stream** - The main information highway in transformers where each layer adds its computations to a continuous stream of representations.

**RequestAnimationFrame** - Browser API used for smooth animation and real-time visualization updates.

## S

**Scaled Dot-Product Attention** - The mathematical operation at the heart of transformer attention: $\text{softmax}\left(\frac{QK^T}{\sqrt{d_k}}\right)V$, where $Q$ are queries, $K$ are keys, $V$ are values, and $d_k$ is the key dimension.

**Sequence Length** - The number of tokens in an input sequence, limited by the model's maximum position embeddings.

**Smalltalk** - The object-oriented programming language that NeuroScope is built in, providing powerful introspection capabilities.

**Softmax** - A function that converts raw scores (logits) into probability distributions.

**SqueakJS** - A browser-based Smalltalk environment that enables NeuroScope to run without installation.

## T

**Tensor** - A multi-dimensional array structure used to store and manipulate numerical data in neural networks.

**Token** - A discrete unit of text (such as a word, subword, or character) that has been converted into a numerical identifier that the model can process. Tokens are the fundamental input units for transformer models.

**Tokenization** - The process of converting text into numerical tokens that can be processed by the model.

**Tokenizer** - A component that handles conversion between text and token representations.

**Transformer** - The neural network architecture that NeuroScope is designed to analyze, based on attention mechanisms.

**TransformerLayer** - An abstract base class representing individual layers in the transformer architecture.

**TransformerModel** - The central class that orchestrates the complete transformer architecture and coordinates all components.

**Typed Arrays** - JavaScript data structures used for efficient numerical computation and memory management.

## V

**Vocabulary Size** - The number of unique tokens that the model can process, determining the size of embedding and output layers.

## W

**WebGL** - Browser graphics API used for GPU-accelerated tensor operations and visualizations.

**WebGLTensor** - A specialized tensor implementation that leverages WebGL for GPU acceleration.

**Web Workers** - Browser API for background processing that enables heavy computations without blocking the user interface.

## Z

**Zero-Installation** - NeuroScope's capability to run complex interpretability analysis directly in browsers without requiring software installation.

---

*This glossary covers the core terminology used throughout the NeuroScope framework. For more detailed information about specific concepts, refer to the relevant documentation files and class comments.*