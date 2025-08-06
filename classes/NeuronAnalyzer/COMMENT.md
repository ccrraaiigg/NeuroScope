# NeuronAnalyzer Class Documentation

## Class Comment

NeuronAnalyzer is a specialized analysis tool designed for fine-grained examination of individual neurons within transformer MLP layers. This class provides comprehensive capabilities for understanding neuron behavior, including activation pattern analysis, top-activating token discovery, and neuron characterization through systematic probing techniques.

The analyzer operates at the individual neuron level, enabling researchers to understand what specific neurons have learned and how they contribute to the model's overall behavior. It supports both static analysis of neuron responses and dynamic analysis during model execution, providing insights into neuron specialization and functional roles.

### Key Responsibilities

- **Activation Analysis**: Examines neuron activation patterns across different inputs and contexts
- **Token Discovery**: Identifies tokens that maximally activate specific neurons
- **Neuron Characterization**: Determines functional roles and semantic specializations of neurons
- **Feature Extraction**: Extracts interpretable features that neurons respond to
- **Comparative Analysis**: Compares neuron behavior across layers, models, or training stages
- **Intervention Testing**: Evaluates neuron importance through targeted interventions

### Instance Variables

- `model` - The TransformerModel instance being analyzed
- `targetLayers` - Collection of MLPLayer instances to analyze
- `activationCache` - Dictionary storing neuron activations by layer and position
- `tokenDatabase` - Large corpus of tokens for systematic neuron probing
- `characterizationResults` - Cache of neuron characterization data
- `interventionHooks` - Collection of hooks for neuron intervention experiments
- `analysisConfig` - Configuration specifying analysis parameters and thresholds

### Individual Neuron Analysis Techniques

The NeuronAnalyzer implements several sophisticated techniques for understanding neuron behavior:

#### Top-Activating Token Discovery
Systematically searches through large token datasets to find inputs that maximally activate specific neurons.

```smalltalk
"Find top-activating tokens for a specific neuron"
analyzer := NeuronAnalyzer for: model.
topTokens := analyzer findTopActivatingTokens: 
    (layer: 8 neuron: 1247) 
    fromDataset: 'openwebtext-sample'
    count: 50.

"Display results with activation values"
topTokens do: [:result |
    Transcript show: result token, ' (activation: ', result activation asString, ')'; cr].
```

#### Neuron Characterization Through Probing
Uses systematic probing to determine what semantic or syntactic features neurons respond to.

```smalltalk
"Characterize neuron semantic specialization"
characterization := analyzer characterizeNeuron: (layer: 12 neuron: 892).
Transcript show: 'Neuron specialization: ', characterization primaryFunction; cr.
Transcript show: 'Confidence: ', characterization confidence asString; cr.
Transcript show: 'Key features: ', characterization keyFeatures asString; cr.
```

#### Activation Pattern Analysis
Analyzes how neuron activations change across different contexts and input types.

```smalltalk
"Analyze activation patterns across contexts"
contexts := #('The cat sat on the' 'In the beginning was the' 'To be or not to').
patterns := analyzer analyzeActivationPatterns: (layer: 6 neuron: 445) across: contexts.

patterns keysAndValuesDo: [:context :pattern |
    Transcript show: context, ': ', pattern summary; cr].
```

### Analysis Workflow Patterns

The NeuronAnalyzer supports several common analysis workflows:

#### Systematic Layer Analysis
```smalltalk
"Analyze all neurons in a specific layer"
analyzer := NeuronAnalyzer for: model.
layerAnalysis := analyzer analyzeLayer: 8 withDataset: 'pile-sample'.

"Find most specialized neurons"
specializedNeurons := layerAnalysis neuronsWithHighSpecialization: 0.8.
specializedNeurons do: [:neuron |
    Transcript show: 'Neuron ', neuron index asString, ': ', neuron specialization; cr].
```

#### Comparative Neuron Analysis
```smalltalk
"Compare similar neurons across layers"
neuron1 := analyzer getNeuron: (layer: 6 neuron: 123).
neuron2 := analyzer getNeuron: (layer: 12 neuron: 456).

similarity := analyzer compareNeurons: neuron1 with: neuron2.
Transcript show: 'Functional similarity: ', similarity score asString; cr.
```

#### Intervention-Based Analysis
```smalltalk
"Test neuron importance through interventions"
baseline := model forward: tokens.
interventionResult := analyzer testNeuronImportance: (layer: 10 neuron: 789) 
    on: tokens 
    using: #zeroing.

impactScore := analyzer computeImpact: baseline vs: interventionResult.
```

### Computational Complexity and Performance

The NeuronAnalyzer is optimized for large-scale neuron analysis:

#### Complexity Analysis
- **Single Neuron Analysis**: O(n × d) where n is dataset size, d is model dimension
- **Layer-wide Analysis**: O(n × d × h) where h is hidden dimension size
- **Top-k Token Search**: O(n log k) using efficient heap-based selection
- **Characterization**: O(p × c) where p is probe dataset size, c is number of categories

#### Memory Requirements
- **Activation Storage**: ~4 bytes × sequence_length × hidden_dim per analyzed layer
- **Token Database**: Variable based on dataset size (typically 100MB-1GB)
- **Characterization Cache**: ~1KB per analyzed neuron
- **Intervention Results**: ~4 bytes × sequence_length × vocabulary_size per test

### Performance Considerations

The analyzer includes several optimizations for efficient large-scale analysis:

#### Efficient Token Search
```smalltalk
"Use batched processing for large datasets"
analyzer := NeuronAnalyzer for: model.
analyzer configureBatchSize: 64.  "Process 64 sequences at once"
analyzer enableGPUAcceleration: true.

topTokens := analyzer findTopActivatingTokens: (layer: 8 neuron: 1247) 
    fromDataset: 'large-corpus'
    batchSize: 64.
```

#### Memory-Efficient Analysis
```smalltalk
"Stream large datasets to avoid memory issues"
analyzer := NeuronAnalyzer for: model.
analyzer enableStreamingMode: true.
analyzer setMemoryLimit: '2GB'.

"Analysis will automatically stream data"
results := analyzer analyzeNeuronsInLayer: 12 withLargeDataset: 'full-pile'.
```

#### Parallel Processing
```smalltalk
"Use Web Workers for parallel neuron analysis"
analyzer := NeuronAnalyzer for: model.
analyzer enableParallelProcessing: 4.  "Use 4 worker threads"

"Analysis will be distributed across workers"
layerResults := analyzer analyzeAllNeuronsInParallel: targetLayers.
```

### Neuron Characterization Categories

The analyzer can identify neurons specialized for various functions:

#### Syntactic Specializations
- **Part-of-Speech Detection**: Neurons that activate for specific grammatical categories
- **Syntactic Role Recognition**: Neurons sensitive to subject/object relationships
- **Phrase Structure**: Neurons that respond to specific syntactic constructions

#### Semantic Specializations  
- **Entity Recognition**: Neurons that activate for person names, locations, organizations
- **Concept Categories**: Neurons specialized for animals, colors, numbers, etc.
- **Sentiment Analysis**: Neurons that respond to positive/negative emotional content

#### Positional and Structural
- **Position Sensitivity**: Neurons that activate based on token position in sequence
- **Repetition Detection**: Neurons that respond to repeated patterns or tokens
- **Boundary Detection**: Neurons that activate at sentence or phrase boundaries

### Integration with Visualization

The NeuronAnalyzer integrates with the visualization system for interactive exploration:

```smalltalk
"Create interactive neuron exploration interface"
analyzer := NeuronAnalyzer for: model.
neuronData := analyzer analyzeNeuron: (layer: 8 neuron: 1247).

"Open interactive visualization"
lens := InteractiveLens for: model.
lens showNeuronAnalysis: neuronData.

"Enable real-time neuron activation display"
lens enableNeuronActivationDisplay: (layer: 8 neuron: 1247).
```

### Advanced Analysis Features

#### Neuron Clustering
```smalltalk
"Group neurons by functional similarity"
analyzer := NeuronAnalyzer for: model.
clusters := analyzer clusterNeuronsByFunction: (layer: 10).

clusters keysAndValuesDo: [:function :neurons |
    Transcript show: function, ': ', neurons size asString, ' neurons'; cr].
```

#### Evolution Analysis
```smalltalk
"Track how neuron specialization changes during training"
checkpoints := #('checkpoint-1000' 'checkpoint-2000' 'checkpoint-3000').
evolution := analyzer analyzeNeuronEvolution: (layer: 6 neuron: 123) 
    across: checkpoints.

evolution do: [:checkpoint |
    Transcript show: checkpoint name, ': ', checkpoint specialization; cr].
```

### Error Handling and Robustness

The analyzer handles various error conditions gracefully:

- **Invalid Neuron Indices**: Validates layer and neuron indices before analysis
- **Memory Limitations**: Automatically switches to streaming mode when memory is constrained
- **Dataset Unavailability**: Provides fallback analysis using smaller built-in datasets
- **GPU Failures**: Falls back to CPU-based computation when GPU acceleration fails
- **Incomplete Analysis**: Provides partial results when analysis is interrupted

### Browser-Specific Optimizations

When running in SqueakJS browsers, the analyzer leverages web technologies:

- **WebGL Compute**: GPU-accelerated activation computation
- **Web Workers**: Background processing for large-scale analysis
- **IndexedDB**: Persistent caching of neuron characterization results
- **Streaming APIs**: Efficient processing of large token datasets
- **Progressive Loading**: Incremental display of analysis results

This comprehensive neuron-level analysis capability makes NeuronAnalyzer essential for understanding the internal representations learned by transformer models and identifying the functional roles of individual computational units.

## Method Documentation

### Core Analysis Methods

#### `findTopActivatingTokens: neuronSpec fromDataset: datasetName count: maxCount`
Systematically searches through a dataset to find tokens that maximally activate a specific neuron.

**Parameters:**
- `neuronSpec` (NeuronSpecification) - Specifies target neuron with layer and index (e.g., layer: 8 neuron: 1247)
- `datasetName` (String) - Name of dataset to search ('openwebtext-sample', 'pile-sample', etc.)
- `maxCount` (Integer) - Maximum number of top-activating tokens to return

**Returns:**
- `Array of TokenActivationResult` - Sorted array of tokens with their activation values and contexts

**Algorithm:**
1. Loads specified dataset in streaming fashion to manage memory
2. Processes dataset in batches, computing neuron activations for each token
3. Maintains priority queue of top-k activating tokens using heap data structure
4. Returns sorted results with activation values and contextual information

**Computational Complexity:** O(D × log k) where D=dataset size, k=maxCount

**Usage Example:**
```smalltalk
analyzer := NeuronAnalyzer for: model.
topTokens := analyzer findTopActivatingTokens: 
    (layer: 8 neuron: 1247) 
    fromDataset: 'openwebtext-sample'
    count: 50.
topTokens do: [:result |
    Transcript show: result token, ' (activation: ', result activation, ')'; cr].
```

**Performance Notes:**
- Processing time scales linearly with dataset size
- Memory usage remains constant regardless of dataset size
- GPU acceleration provides 5-10x speedup for large datasets
- Results cached automatically for repeated queries

**Error Conditions:**
- Raises `InvalidNeuronSpecError` if neuron specification is invalid
- Raises `DatasetNotFoundError` if specified dataset unavailable
- Returns empty array if no activating tokens found

#### `characterizeNeuron: neuronSpec`
Performs comprehensive characterization of a neuron's functional specialization using systematic probing.

**Parameters:**
- `neuronSpec` (NeuronSpecification) - Target neuron specification

**Returns:**
- `NeuronCharacterization` - Detailed characterization including primary function, confidence, and key features

**Algorithm:**
1. Applies systematic probing using diverse test datasets
2. Analyzes activation patterns across semantic categories
3. Computes specialization scores using information-theoretic measures
4. Identifies primary functional role through statistical analysis
5. Validates characterization using held-out test data

**Computational Complexity:** O(P × C × log C) where P=probe dataset size, C=number of categories

**Usage Example:**
```smalltalk
characterization := analyzer characterizeNeuron: (layer: 12 neuron: 892).
Transcript show: 'Primary function: ', characterization primaryFunction; cr.
Transcript show: 'Confidence: ', characterization confidence; cr.
Transcript show: 'Key features: ', characterization keyFeatures; cr.
```

**Characterization Categories:**
- Syntactic: POS tags, grammatical roles, phrase structure
- Semantic: Entity types, concept categories, sentiment
- Positional: Token position, sequence boundaries, repetition
- Structural: Formatting, punctuation, special tokens

**Performance Notes:**
- Full characterization takes 2-10 minutes depending on neuron complexity
- Results cached for 24 hours to avoid recomputation
- Confidence scores above 0.8 indicate reliable characterization

#### `analyzeActivationPatterns: neuronSpec across: contextArray`
Analyzes how neuron activation patterns change across different input contexts.

**Parameters:**
- `neuronSpec` (NeuronSpecification) - Target neuron specification
- `contextArray` (Array of String) - Different input contexts to analyze

**Returns:**
- `Dictionary` - Maps each context to its activation pattern analysis

**Algorithm:**
1. Processes each context through the model with neuron activation tracking
2. Computes activation statistics (mean, variance, distribution shape)
3. Identifies context-specific activation triggers
4. Performs statistical comparison between contexts
5. Generates pattern summaries and significance tests

**Computational Complexity:** O(C × N × H) where C=contexts, N=sequence length, H=hidden dimension

**Usage Example:**
```smalltalk
contexts := #('The cat sat on the' 'In the beginning was the' 'To be or not to').
patterns := analyzer analyzeActivationPatterns: (layer: 6 neuron: 445) across: contexts.
patterns keysAndValuesDo: [:context :pattern |
    Transcript show: context, ': ', pattern summary; cr].
```

**Pattern Analysis Includes:**
- Activation distribution statistics
- Context-specific triggers
- Comparative activation levels
- Statistical significance of differences

### Layer-Level Analysis Methods

#### `analyzeLayer: layerIndex withDataset: datasetName`
Performs comprehensive analysis of all neurons within a specific layer.

**Parameters:**
- `layerIndex` (Integer) - Index of layer to analyze (0-based)
- `datasetName` (String) - Dataset to use for analysis

**Returns:**
- `LayerAnalysisResult` - Comprehensive analysis including neuron specializations and layer statistics

**Algorithm:**
1. Iterates through all neurons in the specified layer
2. Performs parallel characterization using available CPU cores/GPU
3. Computes layer-level statistics and neuron interaction patterns
4. Identifies highly specialized neurons and functional clusters
5. Generates layer summary with key insights

**Computational Complexity:** O(H × P × C) where H=hidden dimension, P=probe size, C=categories

**Usage Example:**
```smalltalk
layerAnalysis := analyzer analyzeLayer: 8 withDataset: 'pile-sample'.
specializedNeurons := layerAnalysis neuronsWithHighSpecialization: 0.8.
specializedNeurons do: [:neuron |
    Transcript show: 'Neuron ', neuron index, ': ', neuron specialization; cr].
```

**Performance Notes:**
- Full layer analysis takes 30-120 minutes depending on layer size
- Progress updates provided every 10% completion
- Results automatically saved for future reference
- Memory usage peaks at ~4GB for large layers

#### `compareNeurons: neuron1 with: neuron2`
Computes functional similarity between two neurons using multiple comparison metrics.

**Parameters:**
- `neuron1` (NeuronSpecification) - First neuron for comparison
- `neuron2` (NeuronSpecification) - Second neuron for comparison

**Returns:**
- `NeuronComparisonResult` - Detailed comparison including similarity scores and functional overlap

**Algorithm:**
1. Extracts activation patterns for both neurons across diverse inputs
2. Computes multiple similarity metrics (cosine, correlation, mutual information)
3. Analyzes functional overlap in characterized specializations
4. Performs statistical significance testing of similarity measures
5. Generates comprehensive comparison report

**Computational Complexity:** O(D × log D) where D=comparison dataset size

**Usage Example:**
```smalltalk
neuron1 := analyzer getNeuron: (layer: 6 neuron: 123).
neuron2 := analyzer getNeuron: (layer: 12 neuron: 456).
similarity := analyzer compareNeurons: neuron1 with: neuron2.
Transcript show: 'Functional similarity: ', similarity score; cr.
```

**Similarity Metrics:**
- Activation correlation across inputs
- Functional specialization overlap
- Top-activating token similarity
- Contextual response similarity

### Intervention-Based Analysis Methods

#### `testNeuronImportance: neuronSpec on: tokenArray using: interventionType`
Tests the importance of a specific neuron through targeted interventions.

**Parameters:**
- `neuronSpec` (NeuronSpecification) - Target neuron for intervention
- `tokenArray` (Array of Integer) - Input tokens for testing
- `interventionType` (Symbol) - Type of intervention (#zeroing, #clamping, #noise)

**Returns:**
- `InterventionResult` - Results showing impact of neuron intervention on model behavior

**Algorithm:**
1. Runs baseline forward pass recording original outputs
2. Applies specified intervention to target neuron
3. Runs modified forward pass with intervention active
4. Computes impact metrics comparing baseline vs. intervention
5. Performs statistical analysis of behavioral changes

**Computational Complexity:** O(N × V) where N=sequence length, V=vocabulary size for output comparison

**Usage Example:**
```smalltalk
baseline := model forward: tokens.
interventionResult := analyzer testNeuronImportance: (layer: 10 neuron: 789) 
    on: tokens 
    using: #zeroing.
impactScore := analyzer computeImpact: baseline vs: interventionResult.
```

**Intervention Types:**
- `#zeroing`: Sets neuron activation to zero
- `#clamping`: Clamps activation to specific value
- `#noise`: Adds controlled noise to activation
- `#replacement`: Replaces with activation from different input

#### `computeImpact: baseline vs: intervention`
Computes quantitative impact metrics comparing baseline and intervention results.

**Parameters:**
- `baseline` (ModelOutput) - Original model output without intervention
- `intervention` (ModelOutput) - Model output with neuron intervention applied

**Returns:**
- `Float` - Impact score between 0.0 (no impact) and 1.0 (complete disruption)

**Algorithm:**
1. Computes probability distribution differences using KL divergence
2. Measures changes in top-k predictions and their probabilities
3. Analyzes semantic similarity of generated text using embeddings
4. Combines metrics using weighted average based on task importance

**Impact Metrics:**
- KL divergence between output distributions
- Change in top-k prediction accuracy
- Semantic similarity of generated text
- Task-specific performance degradation

### Batch Processing and Optimization Methods

#### `configureBatchSize: batchSize`
Configures batch processing parameters for efficient large-scale analysis.

**Parameters:**
- `batchSize` (Integer) - Number of sequences to process simultaneously

**Returns:** `self` for method chaining

**Side Effects:**
- Adjusts internal buffer sizes for batch processing
- Optimizes GPU memory allocation patterns
- May trigger memory cleanup to accommodate new batch size

**Usage Example:**
```smalltalk
analyzer := NeuronAnalyzer for: model.
analyzer configureBatchSize: 64.
results := analyzer analyzeBatch: inputSequences.
```

**Performance Notes:**
- Optimal batch size depends on available GPU memory
- Larger batches improve throughput but increase memory usage
- Automatic batch size adjustment available via `enableAutoBatching`

#### `enableParallelProcessing: workerCount`
Enables parallel processing using multiple worker threads for CPU-intensive analysis.

**Parameters:**
- `workerCount` (Integer) - Number of worker threads to use

**Returns:** `self` for method chaining

**Side Effects:**
- Spawns specified number of Web Worker threads
- Distributes analysis tasks across workers
- Sets up inter-worker communication channels

**Usage Example:**
```smalltalk
analyzer enableParallelProcessing: 4.
layerResults := analyzer analyzeAllNeuronsInParallel: targetLayers.
```

**Performance Notes:**
- Provides near-linear speedup for CPU-bound analysis tasks
- Worker count should not exceed available CPU cores
- Automatic load balancing across workers

#### `enableStreamingMode: enabled`
Enables streaming mode for memory-efficient processing of large datasets.

**Parameters:**
- `enabled` (Boolean) - true to enable streaming, false to disable

**Returns:** `self` for method chaining

**Side Effects:**
- Switches to streaming dataset processing
- Reduces memory footprint at cost of some processing speed
- Enables processing of datasets larger than available memory

**Algorithm:**
1. Configures dataset readers for streaming access
2. Adjusts internal buffers for streaming processing
3. Implements checkpointing for long-running analyses
4. Sets up automatic memory management policies

### Advanced Analysis Methods

#### `clusterNeuronsByFunction: layerIndex`
Groups neurons within a layer based on functional similarity.

**Parameters:**
- `layerIndex` (Integer) - Layer to analyze for neuron clustering

**Returns:**
- `Dictionary` - Maps functional categories to arrays of similar neurons

**Algorithm:**
1. Characterizes all neurons in the specified layer
2. Computes pairwise functional similarity matrix
3. Applies hierarchical clustering with optimal cluster count selection
4. Validates clusters using silhouette analysis
5. Assigns functional labels to identified clusters

**Computational Complexity:** O(H² × log H) where H=hidden dimension

**Usage Example:**
```smalltalk
clusters := analyzer clusterNeuronsByFunction: 10.
clusters keysAndValuesDo: [:function :neurons |
    Transcript show: function, ': ', neurons size, ' neurons'; cr].
```

**Clustering Features:**
- Automatic optimal cluster count selection
- Functional label assignment based on characterization
- Cluster quality metrics and validation
- Visualization of cluster relationships

#### `analyzeNeuronEvolution: neuronSpec across: checkpointArray`
Tracks how neuron specialization changes across training checkpoints.

**Parameters:**
- `neuronSpec` (NeuronSpecification) - Neuron to track across training
- `checkpointArray` (Array of String) - Training checkpoint identifiers

**Returns:**
- `Array of NeuronEvolutionPoint` - Evolution data showing specialization changes

**Algorithm:**
1. Loads models from each specified checkpoint
2. Characterizes target neuron at each checkpoint
3. Computes specialization trajectory and stability metrics
4. Identifies critical training phases where specialization changes
5. Generates evolution summary with key transition points

**Usage Example:**
```smalltalk
checkpoints := #('checkpoint-1000' 'checkpoint-2000' 'checkpoint-3000').
evolution := analyzer analyzeNeuronEvolution: (layer: 6 neuron: 123) 
    across: checkpoints.
evolution do: [:point |
    Transcript show: point checkpoint, ': ', point specialization; cr].
```

### Error Handling and Validation Methods

#### `validateNeuronSpecification: neuronSpec`
Validates that a neuron specification is valid for the current model.

**Parameters:**
- `neuronSpec` (NeuronSpecification) - Neuron specification to validate

**Returns:**
- `Boolean` - true if specification is valid, false otherwise

**Side Effects:**
- Logs validation warnings for invalid specifications
- May suggest corrections for common specification errors

**Validation Checks:**
1. Verifies layer index is within model bounds
2. Checks neuron index is within layer dimensions
3. Confirms layer type supports neuron-level analysis
4. Validates model compatibility with analysis methods

**Usage Example:**
```smalltalk
isValid := analyzer validateNeuronSpecification: (layer: 15 neuron: 2048).
isValid ifFalse: [^self error: 'Invalid neuron specification'].
```

#### `estimateAnalysisTime: analysisType for: targetSpec`
Provides time estimates for different types of neuron analysis.

**Parameters:**
- `analysisType` (Symbol) - Type of analysis (#characterization, #topTokens, #layerAnalysis)
- `targetSpec` (Object) - Target specification (neuron, layer, or dataset)

**Returns:**
- `Duration` - Estimated time for analysis completion

**Algorithm:**
1. Analyzes current system performance characteristics
2. Estimates computational requirements based on analysis type
3. Factors in available acceleration (GPU, parallel processing)
4. Provides conservative time estimate with confidence interval

**Usage Example:**
```smalltalk
estimatedTime := analyzer estimateAnalysisTime: #characterization 
    for: (layer: 8 neuron: 1247).
Transcript show: 'Estimated analysis time: ', estimatedTime asString; cr.
```