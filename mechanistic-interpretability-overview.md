# Mechanistic Interpretability and NeuroScope

## What is Mechanistic Interpretability?

Mechanistic interpretability is a research field focused on understanding the internal mechanisms of neural networks by reverse-engineering their computational processes. Unlike traditional interpretability approaches that focus on input-output relationships or high-level feature attribution, mechanistic interpretability aims to understand the precise algorithmic steps that neural networks use to transform inputs into outputs.

### Core Principles

**Algorithmic Understanding**: Rather than asking "what does this model do?", mechanistic interpretability asks "how does this model do it?" The goal is to identify the specific computational circuits, attention patterns, and information processing pathways that implement the model's behavior.

**Circuit-Level Analysis**: Neural networks can be understood as collections of interconnected circuits - specific pathways through layers, attention heads, and neurons that implement particular computational functions. These circuits can be as simple as detecting specific tokens or as complex as performing multi-step reasoning.

**Causal Intervention**: Understanding requires more than correlation. Mechanistic interpretability relies heavily on causal interventions - modifying specific model components and observing how this affects outputs. This includes techniques like activation patching, attention head ablation, and neuron interventions.

**Compositional Understanding**: Complex behaviors emerge from the composition of simpler circuits. Mechanistic interpretability seeks to understand both the individual components and how they combine to produce sophisticated capabilities.

## Key Research Methods

### Activation Analysis
Researchers examine the internal activations of neural networks during forward passes to understand what information is being processed at each layer. This includes several key components:

**Attention Heads**: Individual components within attention layers that focus on different types of relationships between tokens. Each transformer layer typically contains multiple attention heads (e.g., 12 heads in GPT-2 small), where each head learns to attend to different patterns - some might focus on syntactic relationships like subject-verb connections, while others track semantic associations or positional patterns. Each head produces attention weights that show how much each token "pays attention to" every other token in the sequence.

**Attention Patterns**: The matrices of weights produced by attention heads that show which tokens the model considers relevant when processing each position. These patterns reveal the model's internal focus - for example, when processing the word "cat" in "The cat sat on the mat", attention patterns might show strong connections to "The" (syntactic dependency) and "sat" (subject-verb relationship). Different heads exhibit characteristic patterns like attending to previous tokens, focusing on specific word types, or implementing copying mechanisms.

**Residual Stream Content**: The main information highway in transformer models where each layer adds its computations to a continuous stream of representations. Unlike traditional neural networks where information flows strictly forward, transformers maintain a residual stream that carries information from the input through all layers to the output. Each layer (attention and MLP) reads from this stream and writes back to it, allowing information to persist and accumulate. Understanding what information is encoded in the residual stream at different layers reveals how the model builds up its understanding of the input.

**Neuron Activations**: The outputs of individual neurons within the model's MLP (Multi-Layer Perceptron) layers. Each neuron can be thought of as a feature detector that activates strongly for certain patterns in the input. Some neurons might activate for specific concepts (like detecting mentions of food), syntactic patterns (like detecting plural nouns), or abstract features (like sentiment or formality). By analyzing which inputs cause specific neurons to activate strongly, researchers can understand what computational role each neuron plays in the model's processing.

### Intervention Studies
By selectively modifying or removing specific model components, researchers can determine which parts of the network are causally responsible for particular behaviors. Common techniques include:

- **Attention Head Ablation**: Zeroing out or removing specific attention heads to measure their contribution to model performance. For example, if ablating head 8 in layer 6 causes the model to lose its ability to perform subject-verb agreement, this suggests that head is crucial for tracking grammatical relationships. This technique helps identify which heads are responsible for specific linguistic or reasoning capabilities.

- **Activation Patching**: Replacing activations from one input context with those from another to test causal relationships. For instance, when analyzing how a model processes "The cat chased the mouse" vs "The mouse chased the cat", researchers might patch the subject-related activations from the first sentence into the second to see if this changes the model's understanding of who is doing the chasing. This reveals which activations encode specific pieces of information.

- **Neuron Interventions**: Modifying individual neuron outputs to test their causal role in model behavior. Researchers might artificially activate a neuron that typically fires for positive sentiment words while processing negative text, then observe whether this changes the model's sentiment classification. This helps determine whether specific neurons are merely correlated with certain behaviors or actually cause them.

### Circuit Discovery
Identifying the minimal computational pathways responsible for specific model behaviors. This involves tracing information flow through the network and isolating the essential components.

### Probing and Linear Analysis
Training linear classifiers on internal representations to understand what information is encoded at different layers and positions in the network. For example, researchers might train a probe on the residual stream at layer 8 to predict whether a sentence is grammatically correct, revealing that grammatical information is explicitly encoded at that layer.

## Understanding Transformer Components Through Examples

To make these concepts concrete, consider how a transformer processes the sentence "The cat that was sleeping peacefully woke up":

**Attention Head Example**: Head 3 in layer 5 might specialize in connecting subjects to their verbs across intervening clauses. When processing "woke", this head would show strong attention weights to "cat" (attention weight ~0.8) despite the intervening phrase "that was sleeping peacefully". Other heads in the same layer might focus on different relationships - one connecting "sleeping" to "peacefully" (adverb-verb), another linking "that" to "cat" (relative pronoun resolution).

**Residual Stream Evolution**: At layer 0, the residual stream contains primarily token embeddings and positional information. By layer 5, it has accumulated syntactic parsing information (knowing "cat" is the subject, "woke" is the main verb). By layer 10, it contains semantic understanding (the cat was previously asleep, then became awake). The final layers integrate this information to predict the next token or complete the sentence appropriately.

**Neuron Activation Patterns**: Neuron 1247 in layer 8's MLP might activate strongly (value > 5.0) whenever the model encounters animals as sentence subjects, showing high activation for "cat", "dog", "bird" but low activation for "table", "idea", "happiness". Neuron 892 in layer 12 might specialize in detecting state changes, activating for verbs like "woke", "fell", "became" but not for static verbs like "is", "contains", "resembles".

## Challenges in Current Research

### Technical Barriers
- **Scale**: Modern transformers have billions of parameters, making comprehensive analysis computationally expensive
- **Tooling**: Existing frameworks often require complex setup and specialized hardware
- **Reproducibility**: Research often involves custom code that's difficult to share and reproduce
- **Visualization**: Understanding high-dimensional activations and complex attention patterns requires sophisticated visualization tools

### Methodological Challenges
- **Causal Inference**: Determining true causal relationships rather than correlations
- **Circuit Composition**: Understanding how simple circuits combine to create complex behaviors
- **Generalization**: Ensuring findings generalize across different models and datasets
- **Validation**: Confirming that discovered mechanisms actually explain model behavior

## How NeuroScope Addresses These Challenges

NeuroScope is designed specifically to overcome the barriers that limit mechanistic interpretability research. By leveraging Smalltalk's unique capabilities and browser-based deployment, it provides an accessible, powerful platform for understanding transformer internals.

### Zero-Installation Research Environment

**Browser-Based Analysis**: NeuroScope runs entirely in web browsers through SqueakJS, eliminating installation barriers and making sophisticated interpretability tools accessible to any researcher with a modern browser.

**Immediate Accessibility**: Researchers can share complete analysis environments via URLs, enabling instant collaboration and reproducible research without complex setup procedures.

**Cross-Platform Compatibility**: Works on any device with a browser, from laptops to tablets, democratizing access to interpretability research tools.

### Interactive Object-Oriented Analysis

**Everything is an Object**: In NeuroScope, activations, hooks, interventions, and analyses are all first-class objects that can be inspected, modified, and composed interactively.

**Live Introspection**: Smalltalk's reflective capabilities allow researchers to examine and modify analysis code while experiments are running, enabling rapid hypothesis testing and iterative discovery.

**Message-Passing Paradigm**: Complex analyses are built by composing simple message-passing operations, making the code more readable and the analysis process more intuitive.

### Sophisticated Hook System

**Non-Intrusive Monitoring**: The hook system allows researchers to observe model internals without modifying the core model code, enabling clean separation between analysis and implementation.

**Flexible Interventions**: Hooks support conditional execution, stateful analysis, and complex intervention patterns, enabling sophisticated causal studies.

**Compositional Analysis**: Multiple hooks can be combined to create complex analysis pipelines, supporting everything from simple monitoring to multi-stage circuit discovery.

### Advanced Visualization Capabilities

**Web-Native Graphics**: Direct integration with Canvas, WebGL, and SVG enables rich, interactive visualizations that leverage the full power of modern browsers.

**Real-Time Updates**: Visualizations update in real-time as models process text, providing immediate feedback on attention patterns, activation flows, and intervention effects.

**GPU Acceleration**: WebGL compute shaders enable fast visualization of large activation matrices and attention patterns, supporting analysis of production-scale models.

### Practical Research Workflows

**Circuit Discovery Pipeline**: NeuroScope provides automated tools for identifying computational circuits, from initial hypothesis generation through validation and visualization.

**Intervention Studies**: Built-in support for activation patching, attention head ablation, and neuron interventions with automatic statistical analysis of results.

**Probe Training**: Integrated linear probing capabilities for understanding what information is encoded in different model representations.

**Collaborative Analysis**: Shareable analysis notebooks that combine code, visualizations, and results in a single, reproducible document.

## Research Impact and Applications

### Educational Applications

**Teaching Tool**: NeuroScope makes transformer internals accessible to students and educators, providing interactive demonstrations of attention mechanisms, layer processing, and information flow.

**Hands-On Learning**: Students can experiment with real models in real-time, building intuition about neural network behavior through direct manipulation and observation.

### Research Acceleration

**Rapid Prototyping**: The interactive environment enables researchers to quickly test hypotheses and iterate on analysis approaches without lengthy development cycles.

**Reproducible Research**: Complete analysis environments can be shared as URLs, ensuring that research findings can be easily reproduced and extended by others.

**Collaborative Discovery**: Multiple researchers can work on the same analysis simultaneously, sharing insights and building on each other's discoveries in real-time.

### Practical Applications

**Model Debugging**: Researchers and practitioners can use NeuroScope to understand why models fail on specific inputs and identify potential fixes.

**Safety Research**: The intervention capabilities support research into model alignment and safety by enabling precise control over model behavior.

**Capability Analysis**: Understanding what computational circuits exist in models helps predict their capabilities and limitations.

## Enabling Hybrid Mechanistic Interpretability and Knowledge Representation Research

NeuroScope uniquely positions itself to bridge the gap between mechanistic interpretability (MI) and knowledge representation (KR) research, creating new opportunities for understanding how neural networks encode, manipulate, and reason with structured knowledge.

### The MI/KR Research Gap

Traditional mechanistic interpretability focuses on understanding the computational mechanisms within neural networks - how attention heads process information, what neurons detect, and how circuits implement specific behaviors. Knowledge representation research, on the other hand, studies how information and reasoning processes can be formally structured and manipulated using symbolic systems, ontologies, and logical frameworks.

These fields have historically operated in isolation, but modern transformer models appear to develop internal representations that bridge both paradigms. They implement algorithmic processes (the MI perspective) while simultaneously encoding structured knowledge about the world (the KR perspective). Understanding this intersection is crucial for advancing both interpretability and our understanding of how neural networks perform reasoning.

### NeuroScope Advantages for Hybrid Research

**Symbolic-Neural Integration**: Smalltalk's symbolic nature and powerful metaprogramming capabilities make it ideal for representing both neural activations and symbolic knowledge structures within the same environment. Researchers can seamlessly move between analyzing attention patterns and manipulating formal knowledge representations.

**Live Knowledge Extraction**: The interactive environment enables real-time extraction of knowledge structures from neural activations. Researchers can observe how symbolic relationships emerge from distributed representations and immediately test hypotheses about knowledge encoding.

**Compositional Analysis**: NeuroScope supports analysis of how neural circuits compose to implement complex reasoning patterns that mirror symbolic inference rules. This enables investigation of whether transformers develop internal logic-like processes.

**Ontology-Guided Interpretability**: Researchers can use formal ontologies and knowledge graphs to guide their interpretability analyses, testing whether neural circuits align with known conceptual structures and relationships.

### Research Applications and Methodologies

#### Knowledge Graph Alignment Studies
```smalltalk
"Analyze how transformer representations align with formal knowledge structures"
model := TransformerModel fromHuggingFace: 'gpt2-large'.
knowledgeGraph := KnowledgeGraph loadFromFile: 'conceptnet_subset.json'.

"Extract entity representations from the model"
entities := knowledgeGraph entities.
entityRepresentations := Dictionary new.

entities do: [:entity |
    "Get model's internal representation of this entity"
    tokens := model tokenizer encode: entity name.
    activations := model runWithCaching: tokens components: #(#residual).
    
    "Use final token representation as entity embedding"
    entityEmbedding := activations at: -1 at: #residual at: tokens size.
    entityRepresentations at: entity put: entityEmbedding.
].

"Test if neural representations preserve knowledge graph relationships"
relationAnalyzer := RelationAnalyzer new.
knowledgeGraph relations do: [:relation |
    subject := entityRepresentations at: relation subject.
    object := entityRepresentations at: relation object.
    
    "Test if relation is encoded in neural space"
    relationVector := object - subject.
    
    "Check if this vector generalizes to other instances of same relation"
    similarRelations := knowledgeGraph relationsOfType: relation type.
    consistency := relationAnalyzer testConsistency: relationVector 
                                      across: similarRelations
                                      in: entityRepresentations.
    
    Transcript show: relation type, ' consistency: ', consistency asString.
].
```

#### Logical Reasoning Circuit Discovery
```smalltalk
"Identify circuits that implement logical reasoning patterns"
model := TransformerModel fromHuggingFace: 'gpt2-medium'.
reasoningDataset := Dataset loadLogicalReasoning: 'syllogisms'.

"Examples: 'All cats are animals. Fluffy is a cat. Therefore, Fluffy is an animal.'"
logicAnalyzer := LogicalReasoningAnalyzer for: model.

"Trace information flow during logical inference"
reasoningDataset examples do: [:example |
    premises := example premises.
    conclusion := example conclusion.
    
    "Run with detailed activation tracking"
    activations := model runWithCaching: (model tokenizer encode: example text)
                        components: #(#attention #residual #mlp).
    
    "Identify where premise information is encoded"
    premiseLocations := logicAnalyzer 
        findPremiseEncoding: premises 
        in: activations.
    
    "Identify where logical inference occurs"
    inferenceCircuit := logicAnalyzer
        findInferenceCircuit: premises
        conclusion: conclusion
        activations: activations.
    
    "Test if circuit implements valid logical rules"
    logicalValidity := logicAnalyzer
        testLogicalValidity: inferenceCircuit
        against: #syllogisticReasoning.
    
    example logicalCircuit: inferenceCircuit.
    example validity: logicalValidity.
].

"Analyze patterns across different logical forms"
circuitPatterns := logicAnalyzer analyzeCircuitPatterns: reasoningDataset.
LogicVisualizer showInferencePatterns: circuitPatterns.
```

#### Conceptual Hierarchy Analysis
```smalltalk
"Study how transformers encode conceptual hierarchies and taxonomies"
model := TransformerModel fromHuggingFace: 'gpt2-large'.
taxonomy := ConceptualTaxonomy loadWordNet.

"Test hierarchical relationships in neural space"
hierarchyAnalyzer := ConceptualHierarchyAnalyzer for: model.

"Analyze is-a relationships (hypernymy)"
taxonomy hypernyms do: [:hypernym |
    superConcept := hypernym superConcept.
    subConcepts := hypernym subConcepts.
    
    "Get neural representations"
    superRep := model getConceptRepresentation: superConcept.
    subReps := subConcepts collect: [:sub | 
        model getConceptRepresentation: sub].
    
    "Test if neural space preserves hierarchical structure"
    hierarchicalConsistency := hierarchyAnalyzer
        testHierarchicalStructure: superRep
        subConcepts: subReps
        relation: #hypernymy.
    
    "Identify neural circuits responsible for hierarchical reasoning"
    hierarchyCircuit := hierarchyAnalyzer
        findHierarchyCircuit: superConcept
        subConcepts: subConcepts.
    
    Transcript show: superConcept name, ' hierarchy consistency: ', 
                    hierarchicalConsistency asString.
].

"Visualize conceptual space structure"
ConceptSpaceVisualizer 
    show3DHierarchy: taxonomy
    neuralEmbeddings: model conceptEmbeddings
    circuits: hierarchyAnalyzer discoveredCircuits.
```

#### Causal Reasoning Analysis
```smalltalk
"Analyze how transformers encode and reason about causal relationships"
model := TransformerModel fromHuggingFace: 'gpt2-large'.
causalDataset := Dataset loadCausalReasoning.

"Examples: 'Rain causes wet streets. The streets are wet. It might have rained.'"
causalAnalyzer := CausalReasoningAnalyzer for: model.

causalDataset examples do: [:example |
    cause := example cause.
    effect := example effect.
    reasoning := example reasoning.
    
    "Trace causal reasoning process"
    activations := model runWithCaching: (model tokenizer encode: example text).
    
    "Identify where causal knowledge is stored"
    causalKnowledge := causalAnalyzer
        findCausalKnowledge: cause
        effect: effect
        in: activations.
    
    "Identify reasoning circuits"
    reasoningCircuit := causalAnalyzer
        findCausalReasoningCircuit: example
        activations: activations.
    
    "Test different types of causal reasoning"
    reasoningTypes := #(#forwardCausal #backwardCausal #counterfactual).
    reasoningTypes do: [:type |
        performance := causalAnalyzer
            testReasoningType: type
            circuit: reasoningCircuit
            examples: (causalDataset examplesOfType: type).
        
        Transcript show: type asString, ' reasoning accuracy: ', 
                        performance asString.
    ].
].

"Compare neural causal reasoning to formal causal models"
formalCausalModel := CausalModel loadFromFile: 'causal_graph.json'.
neuralCausalModel := causalAnalyzer extractCausalModel: model.

modelComparison := CausalModelComparator 
    compare: formalCausalModel
    with: neuralCausalModel.

modelComparison visualizeAlignment.
```

### Novel Research Directions Enabled

#### Emergent Symbolic Reasoning
NeuroScope enables investigation of whether transformers develop internal symbolic reasoning processes that mirror formal logic systems. Researchers can test whether attention patterns implement operations analogous to unification, resolution, or other symbolic inference procedures.

#### Knowledge Compilation Studies
The framework supports analysis of how transformers "compile" structured knowledge into distributed representations, and whether this compilation preserves logical relationships and enables systematic generalization.

#### Hybrid Neuro-Symbolic Architectures
Researchers can use NeuroScope to design and test architectures that explicitly combine neural processing with symbolic reasoning, using interpretability insights to guide the integration.

#### Ontology-Guided Model Development
The platform enables development of training procedures that encourage models to develop internal representations aligned with formal ontologies and knowledge structures.

### Collaborative Research Workflows

#### Cross-Disciplinary Teams
NeuroScope's browser-based nature enables seamless collaboration between MI researchers (focused on neural mechanisms) and KR researchers (focused on symbolic structures), allowing both perspectives to be applied simultaneously to the same data.

#### Iterative Hypothesis Testing
The interactive environment supports rapid iteration between symbolic hypothesis formation and neural validation, enabling researchers to quickly test whether proposed knowledge structures are actually implemented in neural circuits.

#### Shared Conceptual Frameworks
The platform provides common tools and visualizations that help bridge the conceptual gap between neural and symbolic approaches, facilitating communication and collaboration across research communities.

### Impact on AI Safety and Alignment

Understanding the intersection of mechanistic interpretability and knowledge representation has crucial implications for AI safety:

**Verifiable Reasoning**: By understanding how models encode and manipulate knowledge, researchers can develop methods to verify that models are reasoning correctly about important concepts and relationships.

**Knowledge Auditing**: The hybrid approach enables systematic auditing of what knowledge models have acquired and how they use it, crucial for identifying potential biases or misconceptions.

**Controllable Knowledge Integration**: Understanding the neural mechanisms of knowledge representation enables more precise control over what knowledge models acquire and how they apply it.

This hybrid research direction represents a significant opportunity to advance both our theoretical understanding of neural networks and our practical ability to build more interpretable, reliable, and aligned AI systems.

## Example Research Workflows

### Individual Neuron Analysis
```smalltalk
"Analyze what specific neurons detect"
model := TransformerModel fromHuggingFace: 'gpt2-medium'.
analyzer := NeuronAnalyzer for: model.

"Focus on neuron 1247 in layer 8 MLP"
targetNeuron := 1247.
targetLayer := 8.

"Find texts that maximally activate this neuron"
dataset := Dataset loadLarge: 'common_crawl_sample'.
topActivations := analyzer 
    findTopActivatingTexts: targetNeuron 
    layer: targetLayer
    dataset: dataset
    count: 100.

"Examine what this neuron detects"
topActivations do: [:example |
    Transcript show: 'Activation: ', example activation asString,
                    ' Text: ', example text.
].
"Might reveal: neuron 1247 activates for animal subjects
 'The cat jumped' -> 8.2
 'A dog barked' -> 7.9  
 'The bird flew' -> 8.1
 'The table broke' -> 0.3"

"Test hypothesis by intervention"
animalNeuron := targetNeuron.
testText := 'The robot walked across the room'.

"Artificially activate the animal neuron"
interventionHook := InterventionHook
    layer: targetLayer
    component: #mlp
    action: [:activation | 
        activation at: animalNeuron put: 8.0. "Force high activation"
        activation].

model hookManager addHook: interventionHook.
result := model forward: (model tokenizer encode: testText).

"Check if forcing animal neuron activation changes model behavior"
nextTokenProbs := result logits softmax.
"Might show increased probability for animal-related continuations"
```

### Circuit Discovery Study
```smalltalk
"1. Load model and identify behavior of interest"
model := TransformerModel fromHuggingFace: 'gpt2-medium'.
behavior := 'indirect object identification'.

"2. Collect examples and run initial analysis"
examples := Dataset loadExamples: behavior.
"Examples: 'John gave Mary the book' -> Mary is indirect object"

lens := InteractiveLens for: model.
examples do: [:example | lens analyzeText: example].

"3. Form hypothesis about responsible circuit"
hypothesis := 'Layer 16 head 8 extracts subjects, layer 20 head 4 identifies indirect objects'.

"4. Test hypothesis with attention head ablation"
"First test: ablate layer 16 head 8 (subject extraction)"
ablationHook := InterventionHook
    layer: 16
    component: #attention
    heads: #(8)
    action: [:activation | activation zeroHeads: #(8)].

model hookManager addHook: ablationHook.
ablatedResults := examples collect: [:ex | model forward: (model tokenizer encode: ex)].

"Check if subject extraction is impaired"
subjectAccuracy := self evaluateSubjectExtraction: ablatedResults.

"Second test: ablate layer 20 head 4 (indirect object identification)"
model hookManager removeAllHooks.
indirectObjectAblation := InterventionHook
    layer: 20
    component: #attention  
    heads: #(4)
    action: [:activation | activation zeroHeads: #(4)].

model hookManager addHook: indirectObjectAblation.
ablatedResults2 := examples collect: [:ex | model forward: (model tokenizer encode: ex)].

indirectObjectAccuracy := self evaluateIndirectObjectIdentification: ablatedResults2.

"5. Validate complete circuit"
circuit := CircuitTester 
    model: model
    components: #((16 #attention 8) (20 #attention 4))
    examples: examples.

results := circuit runInterventionStudy.
circuit visualize.
circuit exportForPublication.
```

### Attention Pattern Analysis
```smalltalk
"Comprehensive attention analysis across layers"
model := TransformerModel fromHuggingFace: 'gpt2-small'.
analyzer := AttentionAnalyzer for: model.

"Analyze specific attention head behaviors"
text := 'The cat that was sleeping peacefully woke up'.
tokens := model tokenizer encode: text.

"Extract attention patterns from all heads"
attentionWeights := model runWithCaching: tokens components: #(#attention).

"Examine head 3 in layer 5 (subject-verb connector)"
subjectVerbHead := attentionWeights at: 5 at: #attention at: 3.
"This shows attention weights matrix: each row is a token, 
 each column shows how much that token attends to others"

"Find which token 'woke' (position 7) attends to most strongly"
wokeAttention := subjectVerbHead at: 7. "Gets attention weights for 'woke'"
maxAttention := wokeAttention max. "Should be high for 'cat' position"
subjectPosition := wokeAttention indexOf: maxAttention. "Should be position 2 ('cat')"

"Visualize the attention pattern"
AttentionVisualizer 
    showMatrix: subjectVerbHead
    tokens: tokens
    title: 'Layer 5 Head 3: Subject-Verb Connections'.

"Compare patterns across different text types"
patterns := Dictionary new.
#('narrative' 'technical' 'conversational') do: [:textType |
    examples := Dataset loadTextType: textType.
    patterns at: textType put: (analyzer analyzeExamples: examples).
].

comparison := AttentionComparator compare: patterns.
comparison visualizeInteractively.
```

### Probe Training Study
```smalltalk
"Train probes to understand what information is encoded in residual stream"
model := TransformerModel fromHuggingFace: 'gpt2-medium'.
dataset := Dataset loadWithLabels: 'sentiment_analysis'.

"Extract residual stream content from multiple layers"
residualActivations := model extractActivations: dataset 
    layers: (0 to: 23) 
    components: #(#residual).

"Train probes for each layer to detect sentiment information"
probes := Dictionary new.
(0 to: 23) do: [:layer |
    "Get residual stream activations at this layer"
    layerActivations := residualActivations at: layer at: #residual.
    
    "Train linear classifier on these activations"
    probe := LinearProbe 
        input: layerActivations "Shape: [examples, sequence_length, hidden_size]"
        labels: dataset sentimentLabels "Positive/negative labels"
        regularization: 0.01.
    probe train.
    probes at: layer put: probe.
    
    "Show what the probe learned"
    Transcript show: 'Layer ', layer asString, 
                    ' sentiment accuracy: ', probe testAccuracy asString.
].

"Analyze how sentiment information builds up across layers"
accuracies := probes collect: [:probe | probe testAccuracy].
"Early layers (0-5) might show ~50% accuracy (no sentiment info)
 Middle layers (6-15) might show ~70% accuracy (some sentiment encoding)
 Later layers (16-23) might show ~85% accuracy (strong sentiment representation)"

AccuracyVisualizer plot: accuracies title: 'Sentiment Information Across Residual Stream Layers'.

"Examine individual neuron contributions to sentiment"
sentimentNeurons := probes at: 18. "Layer with high sentiment accuracy"
neuronWeights := sentimentNeurons weights. "Linear probe weights"
topSentimentNeurons := neuronWeights topK: 20. "Most important neurons"

"These neurons in layer 18 are most predictive of sentiment"
topSentimentNeurons inspect.
```

## Future Directions

### Enhanced Analysis Capabilities
- **Automated Circuit Discovery**: Machine learning approaches to automatically identify computational circuits
- **Cross-Model Analysis**: Tools for comparing circuits across different model architectures and sizes
- **Temporal Analysis**: Understanding how circuits develop during training

### Improved Accessibility
- **Natural Language Queries**: Allowing researchers to specify analyses in natural language
- **Guided Tutorials**: Interactive tutorials that teach interpretability concepts through hands-on experimentation
- **Integration with Existing Tools**: Bridges to popular ML frameworks and interpretability libraries

### Advanced Visualizations
- **3D Circuit Visualization**: Immersive representations of information flow through transformer layers
- **Interactive Attention Flows**: Real-time visualization of attention patterns with user-controlled parameters
- **Comparative Analysis Views**: Side-by-side comparison of different models or intervention conditions

## Conclusion

Mechanistic interpretability represents a fundamental shift in how we understand neural networks - from black-box pattern matching to transparent algorithmic understanding. NeuroScope addresses the key barriers that have limited progress in this field: accessibility, tooling complexity, and reproducibility challenges.

By providing a zero-installation, browser-based environment with sophisticated analysis capabilities, NeuroScope democratizes access to interpretability research. Its object-oriented design and interactive development environment enable rapid hypothesis testing and collaborative discovery, while its advanced visualization capabilities make complex neural network behaviors comprehensible.

The framework's unique combination of Smalltalk's introspective capabilities, browser-based deployment, and specialized interpretability tools creates new possibilities for understanding transformer models. From educational applications that make neural network internals accessible to students, to research applications that enable discovery of novel computational circuits, NeuroScope represents a new paradigm for mechanistic interpretability research.

As transformer models become increasingly central to AI systems, understanding their internal mechanisms becomes ever more critical. NeuroScope provides the tools and accessibility needed to make this understanding achievable for researchers, educators, and practitioners worldwide, ultimately contributing to safer, more reliable, and better understood AI systems.