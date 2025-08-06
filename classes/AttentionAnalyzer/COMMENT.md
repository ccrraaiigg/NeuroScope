# AttentionAnalyzer Class Documentation

## Class Comment

AttentionAnalyzer is a specialized analysis tool that provides comprehensive capabilities for examining attention patterns in transformer models. This class implements sophisticated algorithms for detecting, analyzing, and visualizing attention mechanisms across multiple heads and layers, enabling researchers to understand how models allocate attention during text processing.

The analyzer operates on attention weight matrices extracted from AttentionLayer instances and provides both statistical analysis and pattern recognition capabilities. It supports real-time analysis during model execution through integration with the hook system, as well as batch analysis of cached attention data.

### Key Responsibilities

- **Pattern Detection**: Identifies common attention patterns such as induction heads, copying mechanisms, and syntactic attention
- **Statistical Analysis**: Computes attention entropy, concentration metrics, and head similarity measures  
- **Visualization Generation**: Creates interactive visualizations of attention patterns using Canvas and WebGL rendering
- **Comparative Analysis**: Enables comparison of attention patterns across different inputs, layers, or model variants
- **Performance Optimization**: Implements efficient algorithms for large-scale attention analysis with GPU acceleration

### Instance Variables

- `model` - The TransformerModel instance being analyzed
- `attentionCache` - Dictionary storing cached attention weights by layer and head
- `patternDetectors` - Collection of specialized pattern detection algorithms
- `visualizer` - AttentionVisualizer instance for rendering attention patterns
- `analysisConfig` - Configuration object specifying analysis parameters and thresholds
- `computeDevice` - Device specification for GPU-accelerated computations (WebGL/CPU)

### Pattern Detection Algorithms

The AttentionAnalyzer implements several specialized algorithms for identifying attention patterns:

#### Induction Head Detection
Identifies heads that exhibit the characteristic "look back and copy" pattern where attention focuses on tokens that previously appeared after the current token's context.

```smalltalk
"Detect induction heads in a model"
analyzer := AttentionAnalyzer for: model.
inductionHeads := analyzer detectInductionHeads: 'The cat sat on the mat. The dog sat on the'.
inductionHeads do: [:head | 
    Transcript show: 'Layer ', head layer asString, ' Head ', head index asString; cr].
```

#### Copying Mechanism Analysis  
Detects attention patterns that directly copy information from earlier positions, commonly found in tasks requiring exact token reproduction.

```smalltalk
"Analyze copying patterns"
copyingPatterns := analyzer analyzeCopyingMechanisms: inputTokens.
copyingPatterns keysAndValuesDo: [:position :sources |
    Transcript show: 'Position ', position asString, ' copies from: ', sources asString; cr].
```

#### Syntactic Attention Detection
Identifies heads that attend to syntactically related tokens such as subject-verb relationships or modifier-noun pairs.

```smalltalk
"Detect syntactic attention patterns"
syntacticHeads := analyzer detectSyntacticAttention: 'The quick brown fox jumps over the lazy dog'.
syntacticHeads do: [:pattern |
    Transcript show: pattern relationshipType, ': ', pattern sourceToken, ' -> ', pattern targetToken; cr].
```

### Computational Complexity

The AttentionAnalyzer is designed with performance considerations for large-scale analysis:

- **Attention Weight Processing**: O(n²) per head where n is sequence length
- **Pattern Detection**: O(h × l × n²) where h is heads per layer, l is number of layers
- **Similarity Computation**: O(h²) for head-to-head comparisons within layers
- **Memory Usage**: Approximately 4 bytes × sequence_length² × num_heads × num_layers for full attention caching

### Memory Requirements

For typical model sizes:
- **GPT-2 Small** (12 layers, 12 heads): ~50MB for 512-token sequences
- **GPT-2 Medium** (24 layers, 16 heads): ~200MB for 512-token sequences  
- **GPT-2 Large** (36 layers, 20 heads): ~450MB for 512-token sequences

The analyzer implements memory-efficient streaming analysis for longer sequences and provides options for selective head analysis to reduce memory usage.

### Usage Patterns

#### Basic Attention Analysis
```smalltalk
"Create analyzer and perform basic analysis"
model := TransformerModel fromHuggingFace: 'gpt2-small'.
analyzer := AttentionAnalyzer for: model.

"Analyze attention for specific text"
text := 'The cat sat on the mat'.
attentionData := analyzer analyzeText: text.

"Get attention weights for specific layer and head"
layer5Head3 := attentionData attentionAt: 5 head: 3.
```

#### Real-time Analysis with Hooks
```smalltalk
"Set up real-time attention analysis"
analyzer := AttentionAnalyzer for: model.
analyzer enableRealTimeAnalysis.

"Run model with attention tracking"
output := model forward: tokens.

"Access collected attention patterns"
patterns := analyzer detectedPatterns.
```

#### Comparative Analysis
```smalltalk
"Compare attention patterns between different inputs"
analyzer := AttentionAnalyzer for: model.
baseline := analyzer analyzeText: 'The cat sat on the mat'.
variant := analyzer analyzeText: 'The dog sat on the mat'.

similarity := analyzer compareAttentionPatterns: baseline with: variant.
```

### Integration with Visualization

The AttentionAnalyzer integrates seamlessly with the visualization system:

```smalltalk
"Create interactive attention visualization"
analyzer := AttentionAnalyzer for: model.
attentionData := analyzer analyzeText: 'Your text here'.

"Open interactive lens with attention data"
lens := InteractiveLens for: model.
lens showAttentionAnalysis: attentionData.
```

### Performance Optimization

The analyzer includes several optimization strategies:

- **Lazy Evaluation**: Attention patterns computed only when accessed
- **GPU Acceleration**: WebGL-based computation for large attention matrices
- **Selective Analysis**: Option to analyze only specific heads or layers
- **Caching**: Intelligent caching of frequently accessed attention patterns
- **Streaming**: Memory-efficient processing of long sequences

### Browser Integration

When running in SqueakJS, the AttentionAnalyzer leverages browser capabilities:

- **WebGL Compute Shaders**: GPU-accelerated attention analysis
- **Web Workers**: Background processing for heavy computations
- **IndexedDB**: Persistent storage of analysis results
- **Canvas Rendering**: Real-time visualization updates

### Error Handling

The analyzer handles common error conditions gracefully:

- **Invalid Layer/Head Indices**: Returns nil with descriptive error messages
- **Memory Exhaustion**: Automatically switches to streaming analysis mode
- **GPU Unavailability**: Falls back to CPU-based computation
- **Model Compatibility**: Validates model architecture before analysis

This comprehensive analysis capability makes AttentionAnalyzer essential for understanding transformer attention mechanisms and developing insights into model behavior.

## Method Documentation

### Core Analysis Methods

#### `analyzeText: aString`
Performs comprehensive attention analysis on the provided text input, extracting attention weights from all layers and heads.

**Parameters:**
- `aString` (String) - The input text to analyze, will be tokenized using the model's tokenizer

**Returns:**
- `AttentionAnalysisResult` - Object containing attention weights, pattern classifications, and metadata

**Algorithm:**
1. Tokenizes input text using model tokenizer
2. Runs forward pass with attention weight extraction enabled
3. Applies pattern detection algorithms to extracted weights
4. Computes attention statistics and entropy measures
5. Returns structured analysis results

**Computational Complexity:** O(L × H × N²) where L=layers, H=heads per layer, N=sequence length

**Usage Example:**
```smalltalk
analyzer := AttentionAnalyzer for: model.
result := analyzer analyzeText: 'The cat sat on the mat'.
attentionMatrix := result attentionAt: 5 head: 3.
```

**Performance Notes:**
- Memory usage scales quadratically with sequence length
- GPU acceleration available for sequences > 128 tokens
- Results cached automatically for identical inputs

#### `detectInductionHeads: inputText`
Identifies attention heads exhibiting induction head behavior patterns on the given input.

**Parameters:**
- `inputText` (String) - Text containing repeated patterns for induction head detection

**Returns:**
- `Array of InductionHeadResult` - Collection of detected induction heads with confidence scores

**Algorithm:**
1. Analyzes attention patterns for "look back and copy" behavior
2. Computes induction scores based on attention to previous token occurrences
3. Applies statistical significance testing (p < 0.05)
4. Filters results by confidence threshold (default 0.8)

**Computational Complexity:** O(L × H × N × V) where V=vocabulary size for pattern matching

**Usage Example:**
```smalltalk
inductionHeads := analyzer detectInductionHeads: 'The cat sat. The dog sat. The bird'.
inductionHeads do: [:head | 
    Transcript show: 'Layer ', head layer, ' Head ', head index, ' Score: ', head score; cr].
```

**Performance Notes:**
- Requires minimum sequence length of 20 tokens for reliable detection
- Confidence scores above 0.9 indicate strong induction behavior
- Processing time increases linearly with number of repeated patterns

#### `analyzeCopyingMechanisms: tokenArray`
Detects and analyzes direct copying mechanisms in attention patterns.

**Parameters:**
- `tokenArray` (Array of Integer) - Tokenized input sequence for copying analysis

**Returns:**
- `Dictionary` - Maps target positions to source positions and copying confidence scores

**Algorithm:**
1. Identifies positions with high attention to previous identical tokens
2. Computes copying probability based on attention weight concentration
3. Validates copying behavior through intervention experiments
4. Returns mapping of copy relationships with confidence measures

**Computational Complexity:** O(N² × H) for attention weight analysis plus O(N × I) for interventions

**Usage Example:**
```smalltalk
tokens := model tokenizer encode: 'Repeat: hello world. Again: hello world'.
copyingPatterns := analyzer analyzeCopyingMechanisms: tokens.
copyingPatterns keysAndValuesDo: [:target :sources |
    Transcript show: 'Position ', target, ' copies from positions: ', sources; cr].
```

**Error Conditions:**
- Returns empty dictionary if no copying patterns detected
- Raises `InsufficientDataError` if input sequence too short (< 10 tokens)

#### `detectSyntacticAttention: inputText`
Identifies attention heads that focus on syntactically related tokens.

**Parameters:**
- `inputText` (String) - Text for syntactic attention analysis, preferably grammatically complex

**Returns:**
- `Array of SyntacticAttentionPattern` - Detected syntactic relationships with attention evidence

**Algorithm:**
1. Performs part-of-speech tagging on input text
2. Analyzes attention patterns between syntactically related positions
3. Classifies relationships (subject-verb, modifier-noun, etc.)
4. Computes syntactic attention scores using linguistic features

**Computational Complexity:** O(N × P × H) where P=number of POS tag pairs

**Usage Example:**
```smalltalk
syntacticHeads := analyzer detectSyntacticAttention: 'The quick brown fox jumps over the lazy dog'.
syntacticHeads do: [:pattern |
    Transcript show: pattern relationshipType, ': ', pattern sourceToken, ' -> ', pattern targetToken; cr].
```

**Performance Notes:**
- Requires linguistic preprocessing (POS tagging)
- Accuracy improves with longer, more complex sentences
- Results depend on quality of syntactic parsing

### Comparative Analysis Methods

#### `compareAttentionPatterns: baseline with: variant`
Compares attention patterns between two different inputs or model states.

**Parameters:**
- `baseline` (AttentionAnalysisResult) - Reference attention analysis result
- `variant` (AttentionAnalysisResult) - Comparison attention analysis result

**Returns:**
- `AttentionComparisonResult` - Detailed comparison including similarity scores and difference maps

**Algorithm:**
1. Aligns attention matrices using sequence alignment algorithms
2. Computes cosine similarity between corresponding attention heads
3. Identifies significantly different attention patterns (p < 0.05)
4. Generates difference visualizations and statistical summaries

**Computational Complexity:** O(L × H × N²) for similarity computation

**Usage Example:**
```smalltalk
baseline := analyzer analyzeText: 'The cat sat on the mat'.
variant := analyzer analyzeText: 'The dog sat on the mat'.
comparison := analyzer compareAttentionPatterns: baseline with: variant.
Transcript show: 'Overall similarity: ', comparison overallSimilarity; cr.
```

#### `computeHeadSimilarity: layer1 head: head1 with: layer2 head: head2`
Computes similarity between two specific attention heads across different inputs.

**Parameters:**
- `layer1` (Integer) - Layer index of first attention head
- `head1` (Integer) - Head index within first layer
- `layer2` (Integer) - Layer index of second attention head  
- `head2` (Integer) - Head index within second layer

**Returns:**
- `Float` - Similarity score between 0.0 (completely different) and 1.0 (identical)

**Algorithm:**
1. Extracts attention patterns from specified heads
2. Normalizes attention weights for fair comparison
3. Computes cosine similarity between attention vectors
4. Applies statistical significance testing

**Usage Example:**
```smalltalk
similarity := analyzer computeHeadSimilarity: 5 head: 3 with: 8 head: 7.
Transcript show: 'Head similarity: ', similarity asString; cr.
```

### Real-time Analysis Methods

#### `enableRealTimeAnalysis`
Configures the analyzer for real-time attention tracking during model execution.

**Parameters:** None

**Returns:** `self` for method chaining

**Side Effects:**
- Installs attention extraction hooks in all model layers
- Enables automatic pattern detection during forward passes
- Configures memory management for streaming analysis

**Usage Example:**
```smalltalk
analyzer := AttentionAnalyzer for: model.
analyzer enableRealTimeAnalysis.
output := model forward: tokens.  "Attention automatically tracked"
patterns := analyzer detectedPatterns.
```

**Performance Notes:**
- Adds ~15% overhead to model forward pass
- Memory usage increases by ~2x for attention storage
- Automatic cleanup after analysis completion

#### `disableRealTimeAnalysis`
Disables real-time attention tracking and removes installed hooks.

**Parameters:** None

**Returns:** `self` for method chaining

**Side Effects:**
- Removes all attention extraction hooks
- Clears real-time analysis buffers
- Restores normal model execution performance

### Configuration and Optimization Methods

#### `configureBatchSize: anInteger`
Sets the batch size for efficient processing of multiple inputs.

**Parameters:**
- `anInteger` (Integer) - Batch size, must be positive, recommended range 1-64

**Returns:** `self` for method chaining

**Side Effects:**
- Adjusts internal buffer sizes for batch processing
- Optimizes GPU memory allocation patterns
- May trigger garbage collection to free unused memory

**Usage Example:**
```smalltalk
analyzer configureBatchSize: 32.
results := analyzer analyzeBatch: textArray.
```

**Error Conditions:**
- Raises `InvalidBatchSizeError` if batch size <= 0 or > 128

#### `enableGPUAcceleration: aBoolean`
Enables or disables GPU acceleration for attention analysis.

**Parameters:**
- `aBoolean` (Boolean) - true to enable GPU acceleration, false for CPU-only

**Returns:** `self` for method chaining

**Side Effects:**
- Initializes WebGL compute shaders if enabling GPU
- Transfers existing data to/from GPU memory
- Adjusts algorithm selection based on acceleration availability

**Performance Notes:**
- GPU acceleration provides 3-10x speedup for large sequences
- Requires WebGL 2.0 support in browser
- Automatic fallback to CPU if GPU unavailable

#### `setMemoryLimit: limitString`
Sets memory usage limit for attention analysis operations.

**Parameters:**
- `limitString` (String) - Memory limit specification (e.g., '2GB', '512MB')

**Returns:** `self` for method chaining

**Side Effects:**
- Adjusts caching strategies based on memory limit
- Enables streaming mode if limit is restrictive
- May clear existing caches to respect new limit

**Algorithm:**
1. Parses memory limit string into bytes
2. Estimates current memory usage
3. Adjusts analysis parameters to fit within limit
4. Configures automatic memory management policies

### Visualization Integration Methods

#### `generateAttentionVisualization: analysisResult`
Creates interactive visualization data for attention patterns.

**Parameters:**
- `analysisResult` (AttentionAnalysisResult) - Analysis result to visualize

**Returns:**
- `AttentionVisualizationData` - Structured data for rendering attention patterns

**Algorithm:**
1. Extracts attention weights and metadata from analysis result
2. Computes optimal visualization layout and scaling
3. Generates color mappings and interaction zones
4. Prepares data structures for Canvas/WebGL rendering

**Usage Example:**
```smalltalk
result := analyzer analyzeText: 'Sample text'.
vizData := analyzer generateAttentionVisualization: result.
lens := InteractiveLens for: model.
lens showAttentionVisualization: vizData.
```

### Error Handling and Validation Methods

#### `validateModelCompatibility`
Checks if the current model is compatible with attention analysis.

**Parameters:** None

**Returns:**
- `Boolean` - true if model is compatible, false otherwise

**Side Effects:**
- Logs compatibility warnings to system transcript
- May suggest alternative analysis approaches for incompatible models

**Validation Checks:**
1. Verifies model has attention layers
2. Checks for required hook attachment points
3. Validates attention weight accessibility
4. Confirms tokenizer compatibility

**Usage Example:**
```smalltalk
analyzer := AttentionAnalyzer for: model.
isCompatible := analyzer validateModelCompatibility.
isCompatible ifFalse: [^self error: 'Model not compatible with attention analysis'].
```