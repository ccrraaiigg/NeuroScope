# CircuitFinder Class Documentation

## Class Comment

CircuitFinder is an advanced analysis tool that implements automated algorithms for discovering computational circuits within transformer models. A circuit represents a connected pathway of model components (attention heads, neurons, embeddings) that work together to implement a specific computational function or behavior.

This class provides sophisticated algorithms for identifying, validating, and characterizing circuits through systematic analysis of information flow, activation patterns, and intervention effects. It enables researchers to understand how transformer models decompose complex tasks into modular computational pathways.

### Key Responsibilities

- **Circuit Discovery**: Automatically identifies computational pathways that implement specific behaviors
- **Information Flow Analysis**: Traces how information propagates through model components
- **Circuit Validation**: Verifies discovered circuits through systematic intervention experiments
- **Pathway Characterization**: Determines the functional role and importance of identified circuits
- **Circuit Visualization**: Generates interactive visualizations of discovered computational pathways
- **Comparative Analysis**: Compares circuits across different models, tasks, or training stages

### Instance Variables

- `model` - The TransformerModel instance being analyzed
- `discoveredCircuits` - Dictionary storing identified circuits by function and importance
- `interventionEngine` - Engine for performing systematic circuit validation experiments
- `flowAnalyzer` - Component for tracing information flow through model layers
- `validationResults` - Cache of circuit validation experiment results
- `circuitDatabase` - Repository of known circuit patterns and templates
- `analysisConfig` - Configuration specifying discovery parameters and thresholds

### Automated Circuit Discovery Algorithms

The CircuitFinder implements several sophisticated algorithms for identifying computational circuits:

#### Activation-Based Circuit Discovery
Identifies circuits by analyzing which components consistently activate together for specific input patterns.

```smalltalk
"Discover circuits for induction head behavior"
finder := CircuitFinder for: model.
inductionCircuits := finder discoverCircuitsFor: #inductionHead 
    usingDataset: 'repeated-token-sequences'
    withThreshold: 0.85.

"Display discovered circuits"
inductionCircuits do: [:circuit |
    Transcript show: 'Circuit: ', circuit name; cr.
    Transcript show: 'Components: ', circuit components size asString; cr.
    Transcript show: 'Confidence: ', circuit confidence asString; cr].
```

#### Gradient-Based Circuit Identification
Uses gradient information to identify which components are most important for specific model outputs.

```smalltalk
"Find circuits responsible for next-token prediction"
predictionCircuits := finder discoverCircuitsUsingGradients: 
    targetOutput: 'specific-token-prediction'
    inputExamples: exampleSequences
    gradientThreshold: 0.1.

predictionCircuits do: [:circuit |
    circuit components do: [:component |
        Transcript show: component layer asString, ':', component type, ':', component index asString; cr]].
```

#### Information Flow Circuit Discovery
Traces information flow through the model to identify connected pathways that process specific types of information.

```smalltalk
"Discover circuits for processing syntactic information"
syntaxCircuits := finder traceInformationFlow: 
    fromInput: 'syntactic-test-sentences'
    targetingFeature: #syntacticStructure
    maxDepth: 8.

syntaxCircuits do: [:circuit |
    Transcript show: 'Syntax circuit: ', circuit pathway asString; cr].
```

### Circuit Identification Methods

The CircuitFinder employs multiple complementary approaches for circuit discovery:

#### Template-Based Discovery
Uses known circuit patterns to identify similar structures in new models.

```smalltalk
"Search for known circuit templates"
finder := CircuitFinder for: model.
finder loadCircuitTemplates: 'standard-transformer-circuits'.

knownPatterns := finder findCircuitsMatchingTemplates: 
    templates: #(inductionHead copyingMechanism syntacticAttention)
    similarityThreshold: 0.7.
```

#### Intervention-Based Discovery
Systematically intervenes on model components to identify which combinations are necessary for specific behaviors.

```smalltalk
"Discover circuits through systematic intervention"
behaviorCircuits := finder discoverCircuitsThroughIntervention:
    targetBehavior: #nameMoving
    interventionStrategy: #systematicAblation
    validationDataset: 'name-moving-examples'.

behaviorCircuits do: [:circuit |
    Transcript show: 'Required components: ', circuit essentialComponents asString; cr].
```

#### Causal Mediation Analysis
Uses causal mediation techniques to identify which components mediate the relationship between inputs and specific outputs.

```smallttml
"Find mediating circuits for factual recall"
factualCircuits := finder findMediatingCircuits:
    fromInput: 'factual-prompts'
    toOutput: 'factual-completions'
    mediationThreshold: 0.6.
```

### Circuit Validation Methods

The CircuitFinder includes comprehensive validation techniques to verify discovered circuits:

#### Ablation Validation
Tests circuit importance by systematically removing components and measuring performance degradation.

```smalltalk
"Validate circuit through ablation"
circuit := discoveredCircuits at: #inductionHead.
validationResult := finder validateCircuitByAblation: circuit
    onDataset: 'induction-test-cases'
    measureUsing: #accuracyDrop.

Transcript show: 'Circuit importance: ', validationResult importanceScore asString; cr.
```

#### Activation Patching Validation
Validates circuits by patching activations from different inputs and measuring behavioral changes.

```smalltalk
"Validate using activation patching"
patchingResult := finder validateCircuitByPatching: circuit
    sourceInput: 'clean-example'
    corruptedInput: 'corrupted-example'
    patchingStrategy: #minimalSufficient.

Transcript show: 'Patching restores behavior: ', patchingResult behaviorRestored asString; cr.
```

#### Causal Intervention Validation
Uses sophisticated causal intervention techniques to verify that identified circuits are causally responsible for behaviors.

```smalltalk
"Perform causal validation"
causalResult := finder validateCircuitCausally: circuit
    interventionType: #doCalculus
    confoundingControls: #standardControls
    significanceLevel: 0.05.
```

### Algorithmic Approaches and Computational Requirements

The CircuitFinder implements several algorithmic approaches with different computational trade-offs:

#### Exhaustive Search Algorithm
- **Complexity**: O(2^n) where n is number of model components
- **Memory**: O(n × m) where m is number of test cases
- **Use Case**: Small models or specific component subsets
- **Accuracy**: Highest, finds all possible circuits

#### Greedy Circuit Discovery
- **Complexity**: O(n² × log n) for n components
- **Memory**: O(n × k) where k is maximum circuit size
- **Use Case**: Large models requiring efficient discovery
- **Accuracy**: Good, finds most important circuits

#### Gradient-Guided Search
- **Complexity**: O(n × b × f) where b is batch size, f is forward passes
- **Memory**: O(n × d) where d is model dimension
- **Use Case**: Behavior-specific circuit discovery
- **Accuracy**: High for gradient-sensitive behaviors

#### Information-Theoretic Discovery
- **Complexity**: O(n × m × log m) for mutual information computation
- **Memory**: O(n × m) for activation storage
- **Use Case**: Understanding information flow patterns
- **Accuracy**: Excellent for information-processing circuits

### Computational Requirements

For different model sizes and analysis depths:

#### Small Models (GPT-2 Small)
- **Circuit Discovery**: 10-30 minutes for comprehensive analysis
- **Memory Usage**: 2-4 GB for full circuit database
- **Validation**: 5-15 minutes per discovered circuit

#### Medium Models (GPT-2 Medium/Large)
- **Circuit Discovery**: 1-3 hours for comprehensive analysis
- **Memory Usage**: 8-16 GB for full circuit database
- **Validation**: 15-45 minutes per discovered circuit

#### Large Models (GPT-2 XL and larger)
- **Circuit Discovery**: 3-8 hours for comprehensive analysis
- **Memory Usage**: 16-32 GB for full circuit database
- **Validation**: 30-90 minutes per discovered circuit

### Usage Patterns

#### Basic Circuit Discovery
```smalltalk
"Discover circuits for a specific behavior"
finder := CircuitFinder for: model.
circuits := finder discoverCircuitsFor: #factualRecall
    usingExamples: factualPrompts
    withConfidenceThreshold: 0.8.

"Examine discovered circuits"
circuits do: [:circuit |
    Transcript show: 'Circuit: ', circuit description; cr.
    circuit visualize].
```

#### Comprehensive Model Analysis
```smalltalk
"Perform comprehensive circuit analysis"
finder := CircuitFinder for: model.
finder enableComprehensiveMode.

allCircuits := finder discoverAllCircuits: 
    behaviors: #(inductionHead copyingMechanism factualRecall syntacticProcessing)
    validationLevel: #thorough.

"Generate circuit interaction map"
interactionMap := finder analyzeCircuitInteractions: allCircuits.
```

#### Comparative Circuit Analysis
```smalltalk
"Compare circuits across different models"
finder1 := CircuitFinder for: model1.
finder2 := CircuitFinder for: model2.

circuits1 := finder1 discoverCircuitsFor: #inductionHead.
circuits2 := finder2 discoverCircuitsFor: #inductionHead.

comparison := CircuitFinder compareCircuits: circuits1 with: circuits2.
```

### Circuit Visualization and Interpretation

The CircuitFinder integrates with visualization tools to provide interactive circuit exploration:

```smalltalk
"Create interactive circuit visualization"
circuit := discoveredCircuits at: #inductionHead.
visualization := finder visualizeCircuit: circuit
    withLayout: #hierarchical
    showingActivations: true
    includingFlowArrows: true.

"Open in interactive lens"
lens := InteractiveLens for: model.
lens showCircuitVisualization: visualization.
```

### Advanced Circuit Analysis Features

#### Circuit Composition Analysis
```smalltalk
"Analyze how circuits compose to implement complex behaviors"
compositeCircuits := finder analyzeCircuitComposition:
    primaryCircuit: inductionCircuit
    secondaryCircuits: #(copyingCircuit syntaxCircuit)
    compositionType: #sequential.
```

#### Circuit Evolution Tracking
```smalltalk
"Track how circuits develop during training"
checkpoints := #('step-1000' 'step-5000' 'step-10000').
evolution := finder trackCircuitEvolution: #inductionHead
    acrossCheckpoints: checkpoints
    usingMetric: #circuitStrength.
```

#### Circuit Generalization Analysis
```smalltalk
"Test how circuits generalize across different domains"
generalization := finder testCircuitGeneralization: circuit
    fromDomain: 'natural-language'
    toDomains: #('code' 'mathematics' 'structured-data')
    generalizationMetric: #behaviorPreservation.
```

### Error Handling and Robustness

The CircuitFinder includes comprehensive error handling:

- **Invalid Circuit Specifications**: Validates circuit definitions before analysis
- **Insufficient Data**: Provides warnings when datasets are too small for reliable discovery
- **Memory Limitations**: Automatically switches to memory-efficient algorithms when needed
- **Convergence Failures**: Implements fallback strategies when primary algorithms fail to converge
- **Validation Failures**: Provides detailed diagnostics when circuit validation fails

### Performance Optimization Strategies

#### Parallel Circuit Discovery
```smalltalk
"Use parallel processing for large-scale circuit discovery"
finder := CircuitFinder for: model.
finder enableParallelProcessing: 8.  "Use 8 worker threads"
finder setMemoryPerWorker: '4GB'.

circuits := finder discoverCircuitsInParallel: targetBehaviors.
```

#### Incremental Discovery
```smalltalk
"Build circuits incrementally to manage computational cost"
finder := CircuitFinder for: model.
finder enableIncrementalMode.

"Start with high-confidence components"
coreCircuit := finder findCircuitCore: #inductionHead withConfidence: 0.95.

"Expand circuit with additional components"
expandedCircuit := finder expandCircuit: coreCircuit 
    withConfidenceThreshold: 0.8
    maxExpansionSteps: 5.
```

### Browser Integration and Web Optimization

When running in SqueakJS, the CircuitFinder leverages browser capabilities:

- **WebGL Acceleration**: GPU-accelerated circuit validation experiments
- **Web Workers**: Background circuit discovery without blocking UI
- **IndexedDB Storage**: Persistent caching of discovered circuits
- **Progressive Visualization**: Incremental display of circuit discovery progress
- **Memory Management**: Automatic cleanup of intermediate analysis results

This comprehensive circuit discovery capability makes CircuitFinder essential for understanding the modular computational structure of transformer models and identifying the specific pathways responsible for different model behaviors.

## Method Documentation

### Core Circuit Discovery Methods

#### `discoverCircuitsFor: behaviorType usingDataset: datasetName withThreshold: confidenceThreshold`
Discovers computational circuits responsible for implementing a specific model behavior.

**Parameters:**
- `behaviorType` (Symbol) - Type of behavior to analyze (#inductionHead, #factualRecall, #copyingMechanism, etc.)
- `datasetName` (String) - Dataset containing examples of the target behavior
- `confidenceThreshold` (Float) - Minimum confidence score for circuit inclusion (0.0-1.0)

**Returns:**
- `Array of Circuit` - Discovered circuits sorted by confidence score

**Algorithm:**
1. Loads behavior-specific dataset and extracts relevant examples
2. Performs systematic activation analysis across all model components
3. Identifies components with high correlation to target behavior
4. Applies graph-based clustering to find connected pathways
5. Validates discovered circuits through intervention experiments
6. Filters results by confidence threshold and returns sorted circuits

**Computational Complexity:** O(C × E × log E) where C=components, E=examples

**Usage Example:**
```smalltalk
finder := CircuitFinder for: model.
inductionCircuits := finder discoverCircuitsFor: #inductionHead 
    usingDataset: 'repeated-token-sequences'
    withThreshold: 0.85.
inductionCircuits do: [:circuit |
    Transcript show: 'Circuit: ', circuit name, ' Confidence: ', circuit confidence; cr].
```

**Performance Notes:**
- Discovery time scales with model size and dataset complexity
- GPU acceleration provides 3-5x speedup for large models
- Results cached automatically to avoid recomputation
- Memory usage peaks during component correlation analysis

**Error Conditions:**
- Raises `UnknownBehaviorTypeError` if behavior type not recognized
- Raises `InsufficientDataError` if dataset too small for reliable discovery
- Returns empty array if no circuits meet confidence threshold

#### `discoverCircuitsUsingGradients: targetOutput inputExamples: exampleArray gradientThreshold: threshold`
Uses gradient-based analysis to identify circuits important for specific model outputs.

**Parameters:**
- `targetOutput` (String or Symbol) - Specific output or prediction target
- `exampleArray` (Array) - Input examples that should produce the target output
- `threshold` (Float) - Minimum gradient magnitude for component inclusion

**Returns:**
- `Array of GradientBasedCircuit` - Circuits identified through gradient analysis

**Algorithm:**
1. Computes gradients of target output with respect to all model components
2. Identifies components with gradients above specified threshold
3. Traces gradient flow paths through the model architecture
4. Groups connected high-gradient components into circuit candidates
5. Validates circuit importance through gradient-based interventions
6. Ranks circuits by cumulative gradient magnitude

**Computational Complexity:** O(C × E × D) where C=components, E=examples, D=model dimension

**Usage Example:**
```smalltalk
predictionCircuits := finder discoverCircuitsUsingGradients: 
    targetOutput: 'specific-token-prediction'
    inputExamples: exampleSequences
    gradientThreshold: 0.1.
predictionCircuits do: [:circuit |
    circuit components do: [:component |
        Transcript show: component description, ' Gradient: ', component gradientMagnitude; cr]].
```

**Gradient Analysis Features:**
- Supports both discrete token predictions and continuous outputs
- Handles multi-token target sequences
- Provides gradient magnitude and direction information
- Includes gradient flow visualization data

#### `traceInformationFlow: fromInput targetingFeature: featureType maxDepth: depth`
Traces information flow through the model to identify circuits processing specific features.

**Parameters:**
- `fromInput` (String or Array) - Input text or examples for flow analysis
- `featureType` (Symbol) - Type of information to trace (#syntacticStructure, #semanticContent, #positionalInfo)
- `depth` (Integer) - Maximum depth of flow tracing through model layers

**Returns:**
- `Array of InformationFlowCircuit` - Circuits identified through information flow analysis

**Algorithm:**
1. Performs feature-specific encoding of input information
2. Tracks information propagation through model layers using mutual information
3. Identifies components that preserve or transform target feature information
4. Constructs flow graphs showing information pathways
5. Extracts connected subgraphs as circuit candidates
6. Validates circuits through information-theoretic interventions

**Computational Complexity:** O(L × C × F) where L=layers, C=components per layer, F=feature dimensions

**Usage Example:**
```smalltalk
syntaxCircuits := finder traceInformationFlow: 
    fromInput: 'syntactic-test-sentences'
    targetingFeature: #syntacticStructure
    maxDepth: 8.
syntaxCircuits do: [:circuit |
    Transcript show: 'Syntax circuit: ', circuit pathway; cr].
```

**Information Flow Features:**
- Supports multiple information-theoretic measures
- Provides flow strength and direction information
- Identifies information bottlenecks and amplification points
- Generates interactive flow visualizations

### Template-Based Discovery Methods

#### `findCircuitsMatchingTemplates: templateArray similarityThreshold: threshold`
Searches for circuits matching known templates or patterns from circuit literature.

**Parameters:**
- `templateArray` (Array of Symbol) - Circuit templates to search for
- `threshold` (Float) - Minimum similarity score for template matching

**Returns:**
- `Dictionary` - Maps template names to arrays of matching circuits

**Algorithm:**
1. Loads circuit templates from built-in template database
2. Extracts structural and functional signatures from templates
3. Scans model architecture for components matching template patterns
4. Computes similarity scores using structural and functional metrics
5. Validates matches through template-specific behavioral tests
6. Returns circuits grouped by matched template type

**Usage Example:**
```smalltalk
finder loadCircuitTemplates: 'standard-transformer-circuits'.
knownPatterns := finder findCircuitsMatchingTemplates: 
    templates: #(inductionHead copyingMechanism syntacticAttention)
    similarityThreshold: 0.7.
knownPatterns keysAndValuesDo: [:template :circuits |
    Transcript show: template, ': ', circuits size, ' matches'; cr].
```

**Template Categories:**
- Attention patterns (induction heads, copying mechanisms)
- Syntactic processing circuits
- Factual recall pathways
- Arithmetic computation circuits
- Language modeling circuits

#### `loadCircuitTemplates: templateSource`
Loads circuit templates from a specified source for template-based discovery.

**Parameters:**
- `templateSource` (String) - Source identifier for template collection

**Returns:** `self` for method chaining

**Side Effects:**
- Loads template definitions into internal template database
- Validates template format and completeness
- Indexes templates for efficient matching

**Template Sources:**
- 'standard-transformer-circuits': Common patterns from literature
- 'task-specific-circuits': Templates for specific NLP tasks
- 'custom-templates': User-defined circuit patterns

### Intervention-Based Discovery Methods

#### `discoverCircuitsThroughIntervention: targetBehavior interventionStrategy: strategy validationDataset: datasetName`
Discovers circuits by systematically intervening on model components and measuring behavioral changes.

**Parameters:**
- `targetBehavior` (Symbol) - Specific behavior to analyze through interventions
- `strategy` (Symbol) - Intervention strategy (#systematicAblation, #gradualDisabling, #componentSwapping)
- `datasetName` (String) - Dataset for validating intervention effects

**Returns:**
- `Array of InterventionBasedCircuit` - Circuits discovered through intervention analysis

**Algorithm:**
1. Establishes baseline behavior on validation dataset
2. Systematically applies interventions to individual components
3. Measures behavioral changes using task-specific metrics
4. Identifies components whose intervention significantly affects behavior
5. Tests component combinations to find minimal sufficient circuits
6. Validates discovered circuits through independent intervention tests

**Computational Complexity:** O(C² × E) where C=components, E=evaluation examples

**Usage Example:**
```smalltalk
behaviorCircuits := finder discoverCircuitsThroughIntervention:
    targetBehavior: #nameMoving
    interventionStrategy: #systematicAblation
    validationDataset: 'name-moving-examples'.
behaviorCircuits do: [:circuit |
    Transcript show: 'Required components: ', circuit essentialComponents; cr].
```

**Intervention Strategies:**
- `#systematicAblation`: Removes components individually and in combinations
- `#gradualDisabling`: Progressively reduces component influence
- `#componentSwapping`: Replaces components with alternatives
- `#activationPatching`: Patches activations from different inputs

#### `findMediatingCircuits: fromInput toOutput: targetOutput mediationThreshold: threshold`
Identifies circuits that mediate the relationship between specific inputs and outputs using causal analysis.

**Parameters:**
- `fromInput` (String or Array) - Input stimuli for causal analysis
- `targetOutput` (String or Array) - Target outputs for mediation analysis
- `threshold` (Float) - Minimum mediation effect size for circuit inclusion

**Returns:**
- `Array of MediatingCircuit` - Circuits that significantly mediate input-output relationships

**Algorithm:**
1. Establishes causal baseline between inputs and outputs
2. Systematically intervenes on potential mediating components
3. Measures changes in input-output relationship strength
4. Computes mediation effect sizes using causal inference methods
5. Identifies components with significant mediation effects
6. Constructs circuits from connected mediating components

**Computational Complexity:** O(C × I × O) where C=components, I=inputs, O=outputs

**Usage Example:**
```smalltalk
factualCircuits := finder findMediatingCircuits:
    fromInput: 'factual-prompts'
    toOutput: 'factual-completions'
    mediationThreshold: 0.6.
factualCircuits do: [:circuit |
    Transcript show: 'Mediation strength: ', circuit mediationEffect; cr].
```

### Circuit Validation Methods

#### `validateCircuitByAblation: circuit onDataset: datasetName measureUsing: metric`
Validates a discovered circuit by systematically removing its components and measuring performance impact.

**Parameters:**
- `circuit` (Circuit) - Circuit to validate through ablation
- `datasetName` (String) - Dataset for measuring ablation effects
- `metric` (Symbol) - Performance metric for validation (#accuracyDrop, #perplexityIncrease, #behaviorLoss)

**Returns:**
- `CircuitValidationResult` - Detailed validation results including importance scores

**Algorithm:**
1. Establishes baseline performance on validation dataset
2. Systematically ablates individual circuit components
3. Measures performance changes using specified metric
4. Tests ablation of component combinations
5. Computes component importance scores and circuit necessity
6. Validates results through statistical significance testing

**Computational Complexity:** O(2^C × E) where C=circuit components, E=evaluation examples

**Usage Example:**
```smalltalk
circuit := discoveredCircuits at: #inductionHead.
validationResult := finder validateCircuitByAblation: circuit
    onDataset: 'induction-test-cases'
    measureUsing: #accuracyDrop.
Transcript show: 'Circuit importance: ', validationResult importanceScore; cr.
```

**Validation Metrics:**
- `#accuracyDrop`: Measures decrease in task accuracy
- `#perplexityIncrease`: Measures increase in language modeling perplexity
- `#behaviorLoss`: Measures loss of specific behavioral patterns
- `#outputSimilarity`: Measures similarity to original outputs

#### `validateCircuitByPatching: circuit sourceInput: cleanInput corruptedInput: corruptedInput patchingStrategy: strategy`
Validates circuits using activation patching techniques to test causal necessity.

**Parameters:**
- `circuit` (Circuit) - Circuit to validate through patching
- `cleanInput` (String) - Clean input that produces desired behavior
- `corruptedInput` (String) - Corrupted input that disrupts behavior
- `strategy` (Symbol) - Patching strategy (#minimalSufficient, #maximalNecessary, #componentWise)

**Returns:**
- `PatchingValidationResult` - Results showing behavior restoration through patching

**Algorithm:**
1. Runs model on clean input to capture circuit activations
2. Runs model on corrupted input to establish disrupted baseline
3. Systematically patches circuit component activations from clean to corrupted run
4. Measures behavior restoration after each patching operation
5. Identifies minimal sufficient set of components for behavior restoration
6. Validates results through multiple clean/corrupted input pairs

**Usage Example:**
```smalltalk
patchingResult := finder validateCircuitByPatching: circuit
    sourceInput: 'clean-example'
    corruptedInput: 'corrupted-example'
    patchingStrategy: #minimalSufficient.
Transcript show: 'Patching restores behavior: ', patchingResult behaviorRestored; cr.
```

**Patching Strategies:**
- `#minimalSufficient`: Finds minimal components needed for behavior restoration
- `#maximalNecessary`: Identifies all components that contribute to restoration
- `#componentWise`: Tests each component individually
- `#hierarchical`: Tests components in order of predicted importance

### Advanced Analysis Methods

#### `analyzeCircuitComposition: primaryCircuit secondaryCircuits: circuitArray compositionType: type`
Analyzes how multiple circuits compose to implement complex behaviors.

**Parameters:**
- `primaryCircuit` (Circuit) - Main circuit for composition analysis
- `circuitArray` (Array of Circuit) - Additional circuits that may compose with primary
- `type` (Symbol) - Type of composition to analyze (#sequential, #parallel, #hierarchical)

**Returns:**
- `CircuitCompositionResult` - Analysis of how circuits work together

**Algorithm:**
1. Analyzes activation patterns of all circuits during complex behavior
2. Identifies temporal and spatial relationships between circuits
3. Measures information flow between circuit components
4. Determines composition type and interaction strength
5. Validates composition through targeted interventions
6. Generates composition model with interaction predictions

**Usage Example:**
```smalltalk
compositeCircuits := finder analyzeCircuitComposition:
    primaryCircuit: inductionCircuit
    secondaryCircuits: #(copyingCircuit syntaxCircuit)
    compositionType: #sequential.
Transcript show: 'Composition strength: ', compositeCircuits interactionStrength; cr.
```

#### `trackCircuitEvolution: behaviorType acrossCheckpoints: checkpointArray usingMetric: metric`
Tracks how circuits develop and change during model training.

**Parameters:**
- `behaviorType` (Symbol) - Type of circuit behavior to track
- `checkpointArray` (Array of String) - Training checkpoint identifiers
- `metric` (Symbol) - Metric for measuring circuit development (#circuitStrength, #componentStability, #behaviorFidelity)

**Returns:**
- `CircuitEvolutionResult` - Evolution data showing circuit development over training

**Algorithm:**
1. Loads models from each specified training checkpoint
2. Discovers circuits for target behavior at each checkpoint
3. Aligns circuits across checkpoints using component matching
4. Computes evolution metrics tracking circuit development
5. Identifies critical training phases where circuits emerge or change
6. Generates evolution timeline with key developmental milestones

**Usage Example:**
```smalltalk
checkpoints := #('step-1000' 'step-5000' 'step-10000').
evolution := finder trackCircuitEvolution: #inductionHead
    acrossCheckpoints: checkpoints
    usingMetric: #circuitStrength.
evolution milestones do: [:milestone |
    Transcript show: milestone step, ': ', milestone description; cr].
```

#### `testCircuitGeneralization: circuit fromDomain: sourceDomain toDomains: targetDomains generalizationMetric: metric`
Tests how well discovered circuits generalize across different domains or tasks.

**Parameters:**
- `circuit` (Circuit) - Circuit to test for generalization
- `sourceDomain` (String) - Original domain where circuit was discovered
- `targetDomains` (Array of String) - Target domains for generalization testing
- `metric` (Symbol) - Metric for measuring generalization (#behaviorPreservation, #performanceMaintenance)

**Returns:**
- `CircuitGeneralizationResult` - Results showing generalization performance across domains

**Algorithm:**
1. Establishes circuit performance baseline in source domain
2. Tests circuit behavior on examples from each target domain
3. Measures performance degradation or behavior changes
4. Identifies domain-specific adaptations or failures
5. Computes generalization scores using specified metrics
6. Provides recommendations for circuit adaptation or domain-specific modifications

**Usage Example:**
```smalltalk
generalization := finder testCircuitGeneralization: circuit
    fromDomain: 'natural-language'
    toDomains: #('code' 'mathematics' 'structured-data')
    generalizationMetric: #behaviorPreservation.
generalization results keysAndValuesDo: [:domain :score |
    Transcript show: domain, ': ', score, ' generalization'; cr].
```

### Performance Optimization Methods

#### `enableParallelProcessing: workerCount`
Enables parallel processing for computationally intensive circuit discovery operations.

**Parameters:**
- `workerCount` (Integer) - Number of parallel workers to use

**Returns:** `self` for method chaining

**Side Effects:**
- Spawns specified number of Web Worker threads
- Distributes circuit discovery tasks across workers
- Sets up inter-worker communication and result aggregation

**Usage Example:**
```smalltalk
finder := CircuitFinder for: model.
finder enableParallelProcessing: 8.
circuits := finder discoverCircuitsInParallel: targetBehaviors.
```

#### `enableIncrementalMode`
Enables incremental circuit discovery to manage computational cost for large models.

**Parameters:** None

**Returns:** `self` for method chaining

**Side Effects:**
- Configures discovery algorithms for incremental operation
- Enables checkpointing for long-running discoveries
- Sets up progressive result reporting

**Algorithm:**
1. Starts with high-confidence core components
2. Progressively expands circuits with lower-confidence components
3. Provides intermediate results during discovery process
4. Allows early termination with partial results

**Usage Example:**
```smalltalk
finder enableIncrementalMode.
coreCircuit := finder findCircuitCore: #inductionHead withConfidence: 0.95.
expandedCircuit := finder expandCircuit: coreCircuit 
    withConfidenceThreshold: 0.8
    maxExpansionSteps: 5.
```

### Visualization and Export Methods

#### `visualizeCircuit: circuit withLayout: layoutType showingActivations: showActivations includingFlowArrows: showFlow`
Creates interactive visualization data for discovered circuits.

**Parameters:**
- `circuit` (Circuit) - Circuit to visualize
- `layoutType` (Symbol) - Layout algorithm (#hierarchical, #force-directed, #layered)
- `showActivations` (Boolean) - Whether to include activation strength visualization
- `showFlow` (Boolean) - Whether to include information flow arrows

**Returns:**
- `CircuitVisualizationData` - Structured data for rendering circuit visualization

**Algorithm:**
1. Extracts circuit topology and component relationships
2. Applies specified layout algorithm to position components
3. Computes visualization parameters (colors, sizes, connections)
4. Generates interaction zones and hover information
5. Prepares data structures for Canvas/WebGL rendering

**Usage Example:**
```smalltalk
circuit := discoveredCircuits at: #inductionHead.
visualization := finder visualizeCircuit: circuit
    withLayout: #hierarchical
    showingActivations: true
    includingFlowArrows: true.
lens := InteractiveLens for: model.
lens showCircuitVisualization: visualization.
```

### Error Handling and Diagnostics Methods

#### `validateCircuitDiscoveryParameters: parameters`
Validates parameters for circuit discovery to prevent common configuration errors.

**Parameters:**
- `parameters` (Dictionary) - Discovery parameters to validate

**Returns:**
- `ValidationResult` - Results indicating parameter validity and suggested corrections

**Validation Checks:**
1. Verifies threshold values are within valid ranges
2. Checks dataset availability and format compatibility
3. Validates behavior type specifications
4. Confirms model compatibility with discovery algorithms

**Usage Example:**
```smalltalk
parameters := Dictionary new
    at: #confidenceThreshold put: 0.85;
    at: #datasetName put: 'openwebtext-sample';
    yourself.
validation := finder validateCircuitDiscoveryParameters: parameters.
validation isValid ifFalse: [^self error: validation errorMessage].
```

#### `diagnoseCircuitDiscoveryFailure: failureResult`
Provides diagnostic information when circuit discovery fails or produces unexpected results.

**Parameters:**
- `failureResult` (DiscoveryFailureResult) - Result object from failed discovery attempt

**Returns:**
- `DiagnosticReport` - Detailed analysis of failure causes and suggested remedies

**Diagnostic Categories:**
- Insufficient data quality or quantity
- Model architecture incompatibilities
- Parameter configuration issues
- Computational resource limitations
- Algorithm convergence failures

**Usage Example:**
```smalltalk
circuits := finder discoverCircuitsFor: #complexBehavior usingDataset: 'small-dataset'.
circuits isEmpty ifTrue: [
    diagnostic := finder diagnoseCircuitDiscoveryFailure: finder lastFailureResult.
    Transcript show: 'Discovery failed: ', diagnostic primaryCause; cr.
    diagnostic suggestions do: [:suggestion |
        Transcript show: '  Suggestion: ', suggestion; cr]].
```