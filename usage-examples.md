# NeuroScope Usage Examples

This document provides comprehensive code examples demonstrating typical NeuroScope framework usage patterns, including model loading, forward pass execution, basic activation extraction, error handling, and performance optimization techniques.

## Table of Contents

1. [Model Loading and Basic Operations](#model-loading-and-basic-operations)
2. [Activation Extraction and Analysis](#activation-extraction-and-analysis)
3. [Hook System Usage](#hook-system-usage)
4. [Error Handling Patterns](#error-handling-patterns)
5. [Performance Optimization](#performance-optimization)
6. [Debugging and Troubleshooting](#debugging-and-troubleshooting)

## Model Loading and Basic Operations

### Basic Model Loading

```smalltalk
"Load a pre-trained model from HuggingFace"
model := TransformerModel fromHuggingFace: 'gpt2-small'.

"Verify model loaded correctly"
model ifNil: [
    self error: 'Failed to load model'
].

"Check model configuration"
config := model config.
Transcript show: 'Model loaded: ', (config at: #modelType), ' with ', 
                 (config at: #numLayers) asString, ' layers'.
Transcript show: 'Hidden size: ', (config at: #hiddenSize) asString.
Transcript show: 'Vocabulary size: ', (config at: #vocabSize) asString.
```

### Text Processing and Forward Pass

```smalltalk
"Basic text processing"
inputText := 'The cat sat on the mat and looked around curiously.'.
tokens := model tokenizer encode: inputText.

"Verify tokenization"
Transcript show: 'Input text: ', inputText.
Transcript show: 'Tokens: ', tokens asString.
Transcript show: 'Token count: ', tokens size asString.

"Decode tokens back to verify"
decodedText := model tokenizer decode: tokens.
Transcript show: 'Decoded: ', decodedText.

"Perform forward pass"
output := model forward: tokens.

"Access output components"
logits := output logits.
Transcript show: 'Output logits shape: ', logits shape asString.
Transcript show: 'Final token logits mean: ', logits last mean asString.
```

### Next Token Prediction

```smalltalk
"Get next token predictions"
inputText := 'The future of artificial intelligence is'.
tokens := model tokenizer encode: inputText.
output := model forward: tokens.

"Get probabilities for next token"
finalLogits := output logits last.
probabilities := finalLogits softmax.

"Find top predictions"
topPredictions := probabilities topK: 10.
topValues := topPredictions at: #values.
topIndices := topPredictions at: #indices.

"Display predictions"
Transcript show: 'Top 10 next token predictions:'.
(1 to: 10) do: [:i |
    tokenId := topIndices at: i.
    probability := topValues at: i.
    tokenText := model tokenizer decode: {tokenId}.
    Transcript show: i asString, '. "', tokenText, '" (', 
                     (probability * 100) rounded asString, '%)'
].
```

### Text Generation

```smalltalk
"Simple text generation"
prompt := 'Once upon a time in a distant galaxy'.
generated := model generate: prompt maxTokens: 50.

Transcript show: 'Generated text:'.
Transcript show: generated.

"Generation with custom parameters"
story := model 
    generate: 'The mysterious door opened to reveal'
    maxTokens: 100
    temperature: 0.8
    topP: 0.9.

Transcript show: 'Creative story:'.
Transcript show: story.

"Multiple generation attempts"
prompt := 'The key to happiness is'.
completions := (1 to: 5) collect: [:i |
    model generate: prompt maxTokens: 30 temperature: 1.0
].

Transcript show: 'Multiple completions:'.
completions withIndexDo: [:completion :index |
    Transcript show: index asString, '. ', completion
].
```

## Activation Extraction and Analysis

### Basic Activation Caching

```smalltalk
"Extract activations from specific layers"
inputText := 'The cat sat on the mat'.
tokens := model tokenizer encode: inputText.

"Cache activations from multiple layers"
result := model 
    runWithCaching: tokens
    layers: #(0 6 11)  "First, middle, and last layers"
    components: #(#residual #attention #mlp).

"Access cached activations"
cachedActivations := result cachedActivations.

"Analyze layer 6 residual stream"
layer6Residual := cachedActivations at: (6 -> #residual).
Transcript show: 'Layer 6 residual shape: ', layer6Residual shape asString.
Transcript show: 'Layer 6 residual mean: ', layer6Residual mean asString.
Transcript show: 'Layer 6 residual std: ', layer6Residual std asString.

"Analyze attention patterns"
layer6Attention := cachedActivations at: (6 -> #attention).
Transcript show: 'Layer 6 attention shape: ', layer6Attention shape asString.

"Compare activations across layers"
layer0Residual := cachedActivations at: (0 -> #residual).
layer11Residual := cachedActivations at: (11 -> #residual).

Transcript show: 'Activation evolution:'.
Transcript show: 'Layer 0 mean: ', layer0Residual mean asString.
Transcript show: 'Layer 6 mean: ', layer6Residual mean asString.
Transcript show: 'Layer 11 mean: ', layer11Residual mean asString.
```

### Comprehensive Activation Analysis

```smalltalk
"Comprehensive activation extraction"
analysisText := 'The quick brown fox jumps over the lazy dog'.
tokens := model tokenizer encode: analysisText.

"Cache all components from all layers"
fullResult := model
    runWithCaching: tokens
    layers: (0 to: model config at: #numLayers) - 1
    components: #(#residual #attention #mlp #layerNorm).

cachedActivations := fullResult cachedActivations.

"Analyze activation statistics by layer"
activationStats := Dictionary new.

(0 to: model config at: #numLayers) - 1 do: [:layerIndex |
    layerStats := Dictionary new.
    
    "Residual stream statistics"
    residual := cachedActivations at: (layerIndex -> #residual).
    layerStats at: #residualMean put: residual mean.
    layerStats at: #residualStd put: residual std.
    layerStats at: #residualMax put: residual max.
    
    "MLP statistics if available"
    (cachedActivations includesKey: (layerIndex -> #mlp)) ifTrue: [
        mlp := cachedActivations at: (layerIndex -> #mlp).
        layerStats at: #mlpMean put: mlp mean.
        layerStats at: #mlpActivationRate put: (mlp > 0) mean.
    ].
    
    activationStats at: layerIndex put: layerStats.
].

"Display activation evolution"
Transcript show: 'Activation Statistics by Layer:'.
activationStats keysAndValuesDo: [:layer :stats |
    Transcript show: 'Layer ', layer asString, ':'.
    Transcript show: '  Residual mean: ', (stats at: #residualMean) asString.
    Transcript show: '  Residual std: ', (stats at: #residualStd) asString.
    (stats includesKey: #mlpActivationRate) ifTrue: [
        Transcript show: '  MLP activation rate: ', 
                         ((stats at: #mlpActivationRate) * 100) rounded asString, '%'
    ].
].
```

### Token-Level Analysis

```smalltalk
"Analyze activations at the token level"
sentence := 'The cat sat on the mat'.
tokens := model tokenizer encode: sentence.
tokenTexts := tokens collect: [:tokenId | model tokenizer decode: {tokenId}].

"Extract residual stream activations"
result := model 
    runWithCaching: tokens
    layers: #(6)
    components: #(#residual).

layer6Residual := result cachedActivations at: (6 -> #residual).

"Analyze each token position"
Transcript show: 'Token-level analysis for layer 6:'.
(1 to: tokens size) do: [:tokenPos |
    tokenText := tokenTexts at: tokenPos.
    tokenActivation := layer6Residual slice: {tokenPos - 1} axis: 1.
    
    "Calculate token statistics"
    tokenMean := tokenActivation mean.
    tokenNorm := tokenActivation norm.
    
    Transcript show: 'Token ', tokenPos asString, ' ("', tokenText, '"):'.
    Transcript show: '  Mean activation: ', tokenMean asString.
    Transcript show: '  Activation norm: ', tokenNorm asString.
].

"Find most active tokens"
tokenNorms := (1 to: tokens size) collect: [:pos |
    tokenActivation := layer6Residual slice: {pos - 1} axis: 1.
    tokenActivation norm
].

maxNormIndex := tokenNorms indexOf: tokenNorms max.
maxNormToken := tokenTexts at: maxNormIndex.

Transcript show: 'Most active token: "', maxNormToken, '" at position ', 
                 maxNormIndex asString.
```

## Hook System Usage

### Basic Hook Registration

```smalltalk
"Create and register a simple monitoring hook"
monitorHook := ActivationHook new
    name: 'layer_monitor';
    layer: 5;
    action: [:activation | 
        Transcript show: 'Layer 5 activation - Mean: ', activation mean asString,
                         ', Std: ', activation std asString.
        activation  "Return unmodified for monitoring"
    ];
    yourself.

"Register hook with model"
model hookManager addHook: monitorHook.

"Run forward pass with monitoring"
tokens := model tokenizer encode: 'Hello world'.
output := model forward: tokens.  "Hook executes automatically"

"Remove hook when done"
model hookManager removeHook: 'layer_monitor'.
```

### Activation Extraction Hook

```smalltalk
"Create hook to extract and store activations"
extractedActivations := Dictionary new.

extractionHook := ActivationHook new
    name: 'activation_extractor';
    layer: 8;
    action: [:activation | 
        "Store a copy of the activation"
        extractedActivations at: 'layer8' put: activation copy.
        activation  "Return original"
    ];
    yourself.

model hookManager addHook: extractionHook.

"Process multiple sentences"
sentences := {
    'The cat sat on the mat'.
    'Dogs love to play fetch'.
    'Birds fly high in the sky'.
}.

sentences do: [:sentence |
    tokens := model tokenizer encode: sentence.
    output := model forward: tokens.
    
    "Access extracted activation"
    activation := extractedActivations at: 'layer8'.
    Transcript show: 'Sentence: "', sentence, '"'.
    Transcript show: '  Layer 8 mean: ', activation mean asString.
].

model hookManager removeHook: 'activation_extractor'.
```

### Intervention Hook

```smalltalk
"Create intervention hook to modify activations"
interventionActive := false.
interventionStrength := 0.5.

interventionHook := InterventionHook new
    name: 'activation_scaling';
    layer: 6;
    condition: [:activation | interventionActive];
    action: [:activation | 
        interventionActive 
            ifTrue: [
                Transcript show: 'Applying intervention: scaling by ', 
                                 interventionStrength asString.
                activation * interventionStrength
            ]
            ifFalse: [activation]
    ];
    yourself.

model hookManager addHook: interventionHook.

"Test with and without intervention"
testText := 'The future of AI is bright'.
tokens := model tokenizer encode: testText.

"Baseline (no intervention)"
interventionActive := false.
baselineOutput := model forward: tokens.
baselineLogits := baselineOutput logits last.

"With intervention"
interventionActive := true.
interventionOutput := model forward: tokens.
interventionLogits := interventionOutput logits last.

"Compare results"
logitDifference := (baselineLogits - interventionLogits) abs mean.
Transcript show: 'Average logit difference: ', logitDifference asString.

"Reset"
interventionActive := false.
model hookManager removeHook: 'activation_scaling'.
```

### Multi-Layer Hook Coordination

```smalltalk
"Create coordinated hooks across multiple layers"
layerActivations := Dictionary new.

"Hook to collect activations from multiple layers"
collectionHook := ActivationHook new
    name: 'multi_layer_collector';
    layer: #all;
    action: [:activation :layerIndex | 
        layerActivations at: layerIndex put: activation copy.
        activation
    ];
    yourself.

"Hook to analyze collected activations (runs on final layer)"
analysisHook := ActivationHook new
    name: 'cross_layer_analysis';
    layer: 11;  "Final layer"
    dependsOn: #('multi_layer_collector');
    action: [:activation | 
        "Analyze information flow across layers"
        Transcript show: 'Cross-layer analysis:'.
        
        "Calculate layer-to-layer similarity"
        (1 to: 10) do: [:i |
            layer1 := layerActivations at: i.
            layer2 := layerActivations at: i + 1.
            
            "Cosine similarity between layer representations"
            similarity := (layer1 dot: layer2) / (layer1 norm * layer2 norm).
            Transcript show: 'Layer ', i asString, ' -> ', (i + 1) asString, 
                             ' similarity: ', similarity asString.
        ].
        
        activation
    ];
    yourself.

"Register hooks"
model hookManager 
    addHook: collectionHook;
    addHook: analysisHook.

"Run analysis"
tokens := model tokenizer encode: 'The cat sat on the mat'.
output := model forward: tokens.

"Cleanup"
model hookManager removeAllHooks.
layerActivations := Dictionary new.
```

## Error Handling Patterns

### Basic Error Handling

```smalltalk
"Robust model loading with error handling"
loadModel := [
    [model := TransformerModel fromHuggingFace: 'gpt2-small']
        on: NetworkError
        do: [:error | 
            Transcript show: 'Network error loading model: ', error messageText.
            self error: 'Failed to load model due to network issues'
        ]
        on: ModelNotFoundError
        do: [:error |
            Transcript show: 'Model not found: ', error messageText.
            "Try alternative model"
            model := TransformerModel fromHuggingFace: 'gpt2-medium'
        ]
        on: Error
        do: [:error |
            Transcript show: 'Unexpected error: ', error messageText.
            model := nil
        ]
].

loadModel value.

model ifNil: [
    self error: 'Could not load any model'
].
```

### Forward Pass Error Handling

```smalltalk
"Safe forward pass execution"
safeForward := [:inputTokens |
    [
        "Validate input"
        inputTokens ifNil: [
            self error: 'Input tokens cannot be nil'
        ].
        
        inputTokens isEmpty ifTrue: [
            self error: 'Input tokens cannot be empty'
        ].
        
        "Check sequence length"
        maxLength := model config at: #maxPositionEmbeddings.
        inputTokens size > maxLength ifTrue: [
            Transcript show: 'Warning: Input length (', inputTokens size asString, 
                             ') exceeds maximum (', maxLength asString, '). Truncating.'.
            inputTokens := inputTokens first: maxLength.
        ].
        
        "Perform forward pass"
        model forward: inputTokens
    ]
    on: TokenizationError
    do: [:error |
        Transcript show: 'Tokenization error: ', error messageText.
        nil
    ]
    on: MemoryError
    do: [:error |
        Transcript show: 'Memory error during forward pass: ', error messageText.
        "Try with smaller batch or clear cache"
        model clearCache.
        nil
    ]
    on: Error
    do: [:error |
        Transcript show: 'Forward pass error: ', error messageText.
        nil
    ]
].

"Test safe forward pass"
testText := 'This is a test sentence for error handling'.
tokens := model tokenizer encode: testText.
result := safeForward value: tokens.

result ifNotNil: [
    Transcript show: 'Forward pass successful'
] ifNil: [
    Transcript show: 'Forward pass failed'
].
```

### Hook Error Handling

```smalltalk
"Create robust hook with error handling"
robustHook := ActivationHook new
    name: 'robust_analysis_hook';
    layer: 6;
    action: [:activation | 
        [
            "Potentially error-prone analysis"
            complexAnalysis := self performComplexAnalysis: activation.
            self recordAnalysis: complexAnalysis.
            activation
        ]
        on: AnalysisError
        do: [:error |
            Transcript show: 'Analysis error in hook: ', error messageText.
            "Use fallback analysis"
            fallbackAnalysis := self performSimpleAnalysis: activation.
            self recordAnalysis: fallbackAnalysis.
            activation
        ]
        on: Error
        do: [:error |
            Transcript show: 'Unexpected hook error: ', error messageText.
            "Log error and continue"
            self logError: error.
            activation  "Return unmodified"
        ]
    ];
    yourself.

"Configure hook manager error handling"
model hookManager onHookError: [:hook :error |
    Transcript show: 'Hook "', hook name, '" failed: ', error messageText.
    
    "Decide whether to continue or abort"
    hook isEssential 
        ifTrue: [
            Transcript show: 'Essential hook failed, aborting forward pass'.
            error signal  "Re-raise error"
        ]
        ifFalse: [
            Transcript show: 'Non-essential hook failed, continuing'.
            #continue  "Continue with other hooks"
        ]
].

model hookManager addHook: robustHook.

"Test error handling"
tokens := model tokenizer encode: 'Test error handling'.
[output := model forward: tokens]
    on: Error
    do: [:error |
        Transcript show: 'Forward pass aborted due to essential hook failure: ', 
                         error messageText
    ].
```

## Performance Optimization

### Efficient Batch Processing

```smalltalk
"Process multiple texts efficiently"
texts := {
    'The cat sat on the mat'.
    'Dogs love to play in the park'.
    'Birds fly high in the blue sky'.
    'Fish swim in the deep ocean'.
    'Flowers bloom in the spring garden'.
}.

"Tokenize all texts"
allTokens := texts collect: [:text | model tokenizer encode: text].

"Find maximum length for padding"
maxLength := (allTokens collect: #size) max.

"Pad sequences to same length for batch processing"
paddedTokens := allTokens collect: [:tokens |
    padLength := maxLength - tokens size.
    padLength > 0 
        ifTrue: [tokens, (Array new: padLength withAll: 0)]
        ifFalse: [tokens]
].

"Process as batch (if model supports batching)"
batchResults := paddedTokens collect: [:tokens |
    model forward: tokens
].

"Extract results"
batchResults withIndexDo: [:result :index |
    originalText := texts at: index.
    logits := result logits.
    Transcript show: 'Text ', index asString, ': "', originalText, '"'.
    Transcript show: '  Final logits mean: ', logits last mean asString.
].
```

### Memory-Efficient Processing

```smalltalk
"Process long sequences with memory management"
longText := 'This is a very long text that needs to be processed efficiently without running out of memory. ' repeat: 100.

"Split into chunks"
tokens := model tokenizer encode: longText.
chunkSize := 512.  "Process in chunks of 512 tokens"
chunks := OrderedCollection new.

1 to: tokens size by: chunkSize do: [:start |
    endIndex := (start + chunkSize - 1) min: tokens size.
    chunk := tokens copyFrom: start to: endIndex.
    chunks add: chunk.
].

"Process chunks with memory cleanup"
chunkResults := OrderedCollection new.

chunks withIndexDo: [:chunk :index |
    Transcript show: 'Processing chunk ', index asString, ' of ', chunks size asString.
    
    "Process chunk"
    result := model forward: chunk.
    chunkResults add: result logits last.  "Keep only final logits"
    
    "Clear model cache to free memory"
    model clearCache.
    
    "Force garbage collection periodically"
    index \\ 10 = 0 ifTrue: [
        Smalltalk garbageCollect.
    ].
].

Transcript show: 'Processed ', chunks size asString, ' chunks successfully'.
```

### Hook Performance Optimization

```smalltalk
"Optimize hook performance"
model hookManager 
    enablePerformanceMonitoring;
    optimizeForThroughput;
    setExecutionTimeout: 100.  "100ms timeout per hook"

"Create efficient monitoring hook"
efficientHook := ActivationHook new
    name: 'efficient_monitor';
    layer: 6;
    action: [:activation | 
        "Efficient statistics calculation"
        mean := activation mean.
        std := activation std.
        
        "Store only essential information"
        self recordStatistics: {
            #mean -> mean.
            #std -> std.
            #timestamp -> Time millisecondClockValue.
        } asDictionary.
        
        activation  "Return quickly"
    ];
    yourself.

model hookManager addHook: efficientHook.

"Benchmark hook performance"
startTime := Time millisecondClockValue.
iterations := 100.

iterations timesRepeat: [
    tokens := model tokenizer encode: 'Performance test sentence'.
    output := model forward: tokens.
].

endTime := Time millisecondClockValue.
totalTime := endTime - startTime.
avgTime := totalTime / iterations.

"Get performance metrics"
metrics := model hookManager performanceMetrics.
hookOverhead := metrics at: #hookOverhead ifAbsent: [0].

Transcript show: 'Performance Results:'.
Transcript show: '  Total time: ', totalTime asString, 'ms'.
Transcript show: '  Average per iteration: ', avgTime asString, 'ms'.
Transcript show: '  Hook overhead: ', hookOverhead asString, 'ms'.
Transcript show: '  Hook overhead percentage: ', 
                 ((hookOverhead / totalTime) * 100) rounded asString, '%'.

model hookManager removeAllHooks.
```

### Caching Optimization

```smalltalk
"Optimize activation caching for repeated analysis"
"Enable intelligent caching"
model enableSmartCaching: true.

"Define analysis texts"
analysisTexts := {
    'The cat sat on the mat'.
    'The dog ran in the park'.
    'The bird flew over the tree'.
}.

"First pass - populate cache"
Transcript show: 'First pass (populating cache):'.
startTime := Time millisecondClockValue.

analysisTexts do: [:text |
    tokens := model tokenizer encode: text.
    result := model runWithCaching: tokens 
                    layers: #(0 6 11) 
                    components: #(#residual).
].

firstPassTime := Time millisecondClockValue - startTime.

"Second pass - use cache"
Transcript show: 'Second pass (using cache):'.
startTime := Time millisecondClockValue.

analysisTexts do: [:text |
    tokens := model tokenizer encode: text.
    result := model runWithCaching: tokens 
                    layers: #(0 6 11) 
                    components: #(#residual).
].

secondPassTime := Time millisecondClockValue - startTime.

"Report caching benefits"
speedup := firstPassTime / secondPassTime.
Transcript show: 'Caching Results:'.
Transcript show: '  First pass: ', firstPassTime asString, 'ms'.
Transcript show: '  Second pass: ', secondPassTime asString, 'ms'.
Transcript show: '  Speedup: ', speedup asString, 'x'.

"Cache statistics"
cacheStats := model cacheStatistics.
Transcript show: '  Cache hits: ', (cacheStats at: #hits) asString.
Transcript show: '  Cache misses: ', (cacheStats at: #misses) asString.
Transcript show: '  Cache size: ', (cacheStats at: #size) asString, ' MB'.
```

## Debugging and Troubleshooting

### Model Inspection

```smalltalk
"Inspect model configuration and state"
Transcript show: '=== Model Inspection ==='.

"Basic model information"
config := model config.
Transcript show: 'Model type: ', (config at: #modelType ifAbsent: ['Unknown']).
Transcript show: 'Number of layers: ', (config at: #numLayers) asString.
Transcript show: 'Hidden size: ', (config at: #hiddenSize) asString.
Transcript show: 'Number of attention heads: ', (config at: #numAttentionHeads) asString.
Transcript show: 'Vocabulary size: ', (config at: #vocabSize) asString.
Transcript show: 'Max position embeddings: ', (config at: #maxPositionEmbeddings) asString.

"Layer inspection"
Transcript show: '=== Layer Information ==='.
model layers withIndexDo: [:layer :index |
    Transcript show: 'Layer ', (index - 1) asString, ': ', layer class name.
    
    "Check if layer has attention"
    (layer respondsTo: #attentionHeads) ifTrue: [
        Transcript show: '  Attention heads: ', layer attentionHeads asString.
    ].
    
    "Check if layer has MLP"
    (layer respondsTo: #mlpSize) ifTrue: [
        Transcript show: '  MLP size: ', layer mlpSize asString.
    ].
].

"Hook manager status"
Transcript show: '=== Hook Manager Status ==='.
hookManager := model hookManager.
allHooks := hookManager allHooks.
Transcript show: 'Total hooks registered: ', allHooks size asString.

allHooks do: [:hook |
    Transcript show: '  Hook: ', hook name.
    Transcript show: '    Layer: ', hook layer asString.
    Transcript show: '    Active: ', hook isActive asString.
    Transcript show: '    Execution count: ', hook executionCount asString.
].
```

### Activation Debugging

```smalltalk
"Debug activation flow through the model"
debugText := 'Debug this sentence'.
tokens := model tokenizer encode: debugText.

"Create comprehensive debugging hook"
debugHook := ActivationHook new
    name: 'debug_tracer';
    layer: #all;
    action: [:activation :layerIndex | 
        Transcript show: '--- Layer ', layerIndex asString, ' ---'.
        Transcript show: '  Shape: ', activation shape asString.
        Transcript show: '  Mean: ', activation mean asString.
        Transcript show: '  Std: ', activation std asString.
        Transcript show: '  Min: ', activation min asString.
        Transcript show: '  Max: ', activation max asString.
        
        "Check for problematic values"
        hasNaN := activation data anySatisfy: [:val | val isNaN].
        hasInf := activation data anySatisfy: [:val | val isInfinite].
        
        hasNaN ifTrue: [
            Transcript show: '  WARNING: NaN values detected!'
        ].
        
        hasInf ifTrue: [
            Transcript show: '  WARNING: Infinite values detected!'
        ].
        
        "Check activation magnitude"
        maxAbs := activation data collect: #abs max.
        maxAbs > 100 ifTrue: [
            Transcript show: '  WARNING: Very large activations (max: ', maxAbs asString, ')'
        ].
        
        activation
    ];
    yourself.

model hookManager addHook: debugHook.

"Run with debugging"
Transcript show: '=== Activation Flow Debug ==='.
output := model forward: tokens.

"Check final output"
Transcript show: '=== Final Output ==='.
logits := output logits.
Transcript show: 'Output logits shape: ', logits shape asString.
Transcript show: 'Output logits mean: ', logits mean asString.
Transcript show: 'Output logits std: ', logits std asString.

"Check for output issues"
hasNaNOutput := logits data anySatisfy: [:val | val isNaN].
hasInfOutput := logits data anySatisfy: [:val | val isInfinite].

hasNaNOutput ifTrue: [
    Transcript show: 'ERROR: NaN in final output!'
].

hasInfOutput ifTrue: [
    Transcript show: 'ERROR: Infinite values in final output!'
].

model hookManager removeHook: 'debug_tracer'.
```

### Performance Debugging

```smalltalk
"Debug performance issues"
model hookManager enablePerformanceMonitoring.

"Create performance monitoring hook"
performanceHook := ActivationHook new
    name: 'performance_monitor';
    layer: #all;
    action: [:activation :layerIndex | 
        startTime := Time millisecondClockValue.
        
        "Simulate some processing"
        result := activation copy.
        
        endTime := Time millisecondClockValue.
        processingTime := endTime - startTime.
        
        "Log slow operations"
        processingTime > 10 ifTrue: [
            Transcript show: 'SLOW: Layer ', layerIndex asString, 
                             ' took ', processingTime asString, 'ms'
        ].
        
        result
    ];
    yourself.

model hookManager addHook: performanceHook.

"Run performance test"
testSentences := {
    'Short sentence'.
    'This is a medium length sentence for testing'.
    'This is a much longer sentence that contains many more words and should take more time to process through the neural network layers'.
}.

testSentences withIndexDo: [:sentence :index |
    Transcript show: '=== Performance Test ', index asString, ' ==='.
    Transcript show: 'Sentence: "', sentence, '"'.
    
    tokens := model tokenizer encode: sentence.
    
    startTime := Time millisecondClockValue.
    output := model forward: tokens.
    endTime := Time millisecondClockValue.
    
    totalTime := endTime - startTime.
    Transcript show: 'Total time: ', totalTime asString, 'ms'.
    Transcript show: 'Tokens processed: ', tokens size asString.
    Transcript show: 'Time per token: ', (totalTime / tokens size) asString, 'ms'.
].

"Get detailed performance metrics"
metrics := model hookManager performanceMetrics.
Transcript show: '=== Performance Summary ==='.
Transcript show: 'Total hook execution time: ', (metrics at: #totalExecutionTime) asString, 'ms'.
Transcript show: 'Average hook execution time: ', (metrics at: #averageExecutionTime) asString, 'ms'.
Transcript show: 'Slowest hook: ', (metrics at: #slowestHook) name.
Transcript show: 'Hook overhead: ', (metrics at: #hookOverhead) asString, 'ms'.

model hookManager removeAllHooks.
```

### Common Troubleshooting Scenarios

```smalltalk
"Troubleshooting helper methods"

"1. Check if model is properly loaded"
checkModelLoaded := [
    model ifNil: [
        Transcript show: 'ERROR: Model is nil - not loaded properly'.
        ^false
    ].
    
    model config ifNil: [
        Transcript show: 'ERROR: Model config is nil - incomplete loading'.
        ^false
    ].
    
    model tokenizer ifNil: [
        Transcript show: 'ERROR: Tokenizer is nil - tokenization will fail'.
        ^false
    ].
    
    Transcript show: 'Model appears to be loaded correctly'.
    true
].

"2. Validate input tokens"
validateTokens := [:tokens |
    tokens ifNil: [
        Transcript show: 'ERROR: Tokens are nil'.
        ^false
    ].
    
    tokens isEmpty ifTrue: [
        Transcript show: 'ERROR: Token array is empty'.
        ^false
    ].
    
    "Check for invalid token IDs"
    vocabSize := model config at: #vocabSize.
    invalidTokens := tokens select: [:token | 
        token < 0 or: [token >= vocabSize]
    ].
    
    invalidTokens notEmpty ifTrue: [
        Transcript show: 'ERROR: Invalid token IDs found: ', invalidTokens asString.
        ^false
    ].
    
    "Check sequence length"
    maxLength := model config at: #maxPositionEmbeddings.
    tokens size > maxLength ifTrue: [
        Transcript show: 'WARNING: Sequence length (', tokens size asString, 
                         ') exceeds maximum (', maxLength asString, ')'.
    ].
    
    Transcript show: 'Tokens appear valid'.
    true
].

"3. Test basic functionality"
testBasicFunctionality := [
    Transcript show: '=== Basic Functionality Test ==='.
    
    "Test model loading"
    checkModelLoaded value ifFalse: [^false].
    
    "Test tokenization"
    testText := 'Hello world'.
    [tokens := model tokenizer encode: testText]
        on: Error
        do: [:error |
            Transcript show: 'ERROR: Tokenization failed: ', error messageText.
            ^false
        ].
    
    "Validate tokens"
    (validateTokens value: tokens) ifFalse: [^false].
    
    "Test forward pass"
    [output := model forward: tokens]
        on: Error
        do: [:error |
            Transcript show: 'ERROR: Forward pass failed: ', error messageText.
            ^false
        ].
    
    "Test output"
    output ifNil: [
        Transcript show: 'ERROR: Forward pass returned nil'.
        ^false
    ].
    
    output logits ifNil: [
        Transcript show: 'ERROR: Output logits are nil'.
        ^false
    ].
    
    Transcript show: 'Basic functionality test PASSED'.
    true
].

"Run troubleshooting"
testBasicFunctionality value.
```

This comprehensive set of examples demonstrates the key patterns for using NeuroScope effectively, with proper error handling, performance optimization, and debugging techniques. These examples serve as both documentation and practical templates for common interpretability workflows.

## Advanced Analysis Workflows

### Circuit Discovery

```smalltalk
"Automated circuit discovery for specific behaviors"

"Initialize circuit finder"
circuitFinder := CircuitFinder new
    model: model;
    targetBehavior: 'indirect_object_identification';
    searchDepth: 3;
    confidenceThreshold: 0.8;
    yourself.

"Define test cases for the target behavior"
testCases := {
    {
        'input' -> 'John gave Mary the book'.
        'expected' -> 'Mary'.
        'description' -> 'Simple indirect object'
    } asDictionary.
    {
        'input' -> 'The teacher showed the students a diagram'.
        'expected' -> 'the students'.
        'description' -> 'Complex indirect object'
    } asDictionary.
    {
        'input' -> 'She handed him the keys quickly'.
        'expected' -> 'him'.
        'description' -> 'Pronoun indirect object'
    } asDictionary.
}.

"Run circuit discovery"
Transcript show: '=== Circuit Discovery Analysis ==='.
discoveredCircuits := circuitFinder discoverCircuits: testCases.

"Analyze discovered circuits"
discoveredCircuits do: [:circuit |
    Transcript show: 'Circuit: ', circuit name.
    Transcript show: '  Confidence: ', (circuit confidence * 100) rounded asString, '%'.
    Transcript show: '  Components:'.
    
    circuit components do: [:component |
        Transcript show: '    Layer ', component layer asString, 
                         ' - ', component componentType, 
                         ' (', component neurons asString, ' neurons)'.
    ].
    
    Transcript show: '  Pathway:'.
    circuit pathway do: [:connection |
        Transcript show: '    ', connection source, ' -> ', connection target, 
                         ' (strength: ', connection strength asString, ')'.
    ].
].

"Validate circuits with additional test cases"
validationCases := {
    'The chef offered the customer a special dish'.
    'Mom read the children a bedtime story'.
    'The company sent investors quarterly reports'.
}.

Transcript show: '=== Circuit Validation ==='.
validationResults := circuitFinder validateCircuits: discoveredCircuits 
                                    withCases: validationCases.

validationResults do: [:result |
    Transcript show: 'Circuit: ', result circuit name.
    Transcript show: '  Validation accuracy: ', (result accuracy * 100) rounded asString, '%'.
    Transcript show: '  Successful cases: ', result successfulCases size asString, 
                     ' / ', result totalCases asString.
].

"Export circuit for further analysis"
bestCircuit := discoveredCircuits detectMax: [:circuit | circuit confidence].
circuitData := circuitFinder exportCircuit: bestCircuit.

"Save circuit visualization"
visualizer := CircuitVisualizer new.
circuitDiagram := visualizer createDiagram: bestCircuit.
circuitDiagram saveAs: 'indirect_object_circuit.svg'.
```

### Probe Training and Analysis

```smalltalk
"Train linear probes to detect specific linguistic features"

"Define probe training setup"
probeTrainer := LinearProbe new
    model: model;
    targetLayer: 8;
    targetComponent: #residual;
    probeType: #classification;
    yourself.

"Create training dataset for part-of-speech detection"
trainingData := {
    {'The cat sat on the mat' -> #('DET' 'NOUN' 'VERB' 'PREP' 'DET' 'NOUN')}.
    {'Dogs love to play fetch' -> #('NOUN' 'VERB' 'PREP' 'VERB' 'NOUN')}.
    {'She quickly ran home' -> #('PRON' 'ADV' 'VERB' 'NOUN')}.
    {'The beautiful flowers bloom' -> #('DET' 'ADJ' 'NOUN' 'VERB')}.
    {'Children are playing outside' -> #('NOUN' 'VERB' 'VERB' 'ADV')}.
    {'The old man walked slowly' -> #('DET' 'ADJ' 'NOUN' 'VERB' 'ADV')}.
    {'Birds fly high above' -> #('NOUN' 'VERB' 'ADV' 'PREP')}.
    {'Students study hard for exams' -> #('NOUN' 'VERB' 'ADV' 'PREP' 'NOUN')}.
} collect: [:pair | 
    {
        'text' -> pair key.
        'labels' -> pair value.
    } asDictionary
].

"Extract activations for training"
Transcript show: '=== Extracting Training Activations ==='.
trainingActivations := OrderedCollection new.
trainingLabels := OrderedCollection new.

trainingData do: [:example |
    text := example at: 'text'.
    labels := example at: 'labels'.
    
    tokens := model tokenizer encode: text.
    
    "Extract activations from target layer"
    result := model runWithCaching: tokens 
                    layers: #(8) 
                    components: #(#residual).
    
    layerActivations := result cachedActivations at: (8 -> #residual).
    
    "Add each token's activation and label"
    (1 to: tokens size min: labels size) do: [:tokenIndex |
        tokenActivation := layerActivations slice: {tokenIndex - 1} axis: 1.
        tokenLabel := labels at: tokenIndex.
        
        trainingActivations add: tokenActivation.
        trainingLabels add: tokenLabel.
    ].
].

Transcript show: 'Collected ', trainingActivations size asString, ' training examples'.

"Train the probe"
Transcript show: '=== Training Linear Probe ==='.
probeTrainer 
    trainingData: trainingActivations
    labels: trainingLabels
    validationSplit: 0.2
    epochs: 100
    learningRate: 0.001.

trainingResults := probeTrainer train.

Transcript show: 'Training completed:'.
Transcript show: '  Final training accuracy: ', 
                 (trainingResults trainingAccuracy * 100) rounded asString, '%'.
Transcript show: '  Final validation accuracy: ', 
                 (trainingResults validationAccuracy * 100) rounded asString, '%'.
Transcript show: '  Training loss: ', trainingResults finalLoss asString.

"Test probe on new sentences"
testSentences := {
    'The quick brown fox jumps'.
    'Scientists discovered new species'.
    'Technology advances rapidly today'.
}.

Transcript show: '=== Testing Trained Probe ==='.
testSentences do: [:sentence |
    tokens := model tokenizer encode: sentence.
    result := model runWithCaching: tokens 
                    layers: #(8) 
                    components: #(#residual).
    
    layerActivations := result cachedActivations at: (8 -> #residual).
    
    "Predict POS tags for each token"
    predictions := OrderedCollection new.
    (1 to: tokens size) do: [:tokenIndex |
        tokenActivation := layerActivations slice: {tokenIndex - 1} axis: 1.
        prediction := probeTrainer predict: tokenActivation.
        predictions add: prediction.
    ].
    
    "Display results"
    tokenTexts := tokens collect: [:tokenId | model tokenizer decode: {tokenId}].
    Transcript show: 'Sentence: "', sentence, '"'.
    (1 to: tokenTexts size) do: [:i |
        tokenText := tokenTexts at: i.
        prediction := predictions at: i.
        Transcript show: '  "', tokenText, '" -> ', prediction.
    ].
].

"Analyze probe weights to understand what features it learned"
probeWeights := probeTrainer getWeights.
featureImportance := probeTrainer analyzeFeatureImportance.

Transcript show: '=== Probe Analysis ==='.
Transcript show: 'Most important features for POS detection:'.
topFeatures := featureImportance topK: 10.
topFeatures do: [:feature |
    Transcript show: '  Feature ', feature index asString, 
                     ': importance = ', feature importance asString.
].
```

### Intervention Analysis and Causal Tracing

```smalltalk
"Perform systematic intervention analysis to understand causal relationships"

"Define intervention experiment"
interventionExperiment := InterventionExperiment new
    model: model;
    baselineText: 'The cat sat on the mat';
    interventionText: 'The dog sat on the mat';
    targetMetric: #logitDifference;
    yourself.

"Set up intervention targets"
interventionTargets := {
    {'layer' -> 3. 'component' -> #attention. 'description' -> 'Early attention'}.
    {'layer' -> 6. 'component' -> #residual. 'description' -> 'Middle residual'}.
    {'layer' -> 9. 'component' -> #mlp. 'description' -> 'Late MLP'}.
    {'layer' -> 11. 'component' -> #attention. 'description' -> 'Final attention'}.
} collect: #asDictionary.

"Run systematic interventions"
Transcript show: '=== Causal Intervention Analysis ==='.
interventionResults := OrderedCollection new.

interventionTargets do: [:target |
    layer := target at: 'layer'.
    component := target at: 'component'.
    description := target at: 'description'.
    
    Transcript show: 'Testing intervention: ', description.
    
    "Get baseline activations"
    baselineTokens := model tokenizer encode: interventionExperiment baselineText.
    baselineResult := model runWithCaching: baselineTokens 
                            layers: {layer} 
                            components: {component}.
    
    baselineActivation := baselineResult cachedActivations at: (layer -> component).
    
    "Get intervention activations"
    interventionTokens := model tokenizer encode: interventionExperiment interventionText.
    interventionResult := model runWithCaching: interventionTokens 
                                layers: {layer} 
                                components: {component}.
    
    interventionActivation := interventionResult cachedActivations at: (layer -> component).
    
    "Create patching hook"
    patchingHook := InterventionHook new
        name: 'causal_patch';
        layer: layer;
        action: [:activation | 
            "Replace with intervention activation"
            interventionActivation copy
        ];
        yourself.
    
    "Test causal effect"
    model hookManager addHook: patchingHook.
    
    "Run baseline with intervention"
    patchedResult := model forward: baselineTokens.
    patchedLogits := patchedResult logits.
    
    model hookManager removeHook: 'causal_patch'.
    
    "Calculate effect size"
    originalResult := model forward: baselineTokens.
    originalLogits := originalResult logits.
    
    logitDifference := (patchedLogits - originalLogits) abs mean.
    
    "Store results"
    result := {
        'target' -> target.
        'effectSize' -> logitDifference.
        'description' -> description.
    } asDictionary.
    
    interventionResults add: result.
    
    Transcript show: '  Effect size: ', logitDifference asString.
].

"Analyze intervention results"
Transcript show: '=== Intervention Analysis Results ==='.
sortedResults := interventionResults asSortedCollection: [:a :b | 
    (a at: 'effectSize') > (b at: 'effectSize')
].

Transcript show: 'Interventions ranked by causal effect:'.
sortedResults withIndexDo: [:result :rank |
    target := result at: 'target'.
    effectSize := result at: 'effectSize'.
    description := result at: 'description'.
    
    Transcript show: rank asString, '. ', description, 
                     ' (Layer ', (target at: 'layer') asString, 
                     ', ', (target at: 'component'), '): ', 
                     effectSize asString.
].

"Identify critical components"
criticalThreshold := 0.1.
criticalComponents := sortedResults select: [:result | 
    (result at: 'effectSize') > criticalThreshold
].

Transcript show: 'Critical components (effect > ', criticalThreshold asString, '):'.
criticalComponents do: [:result |
    target := result at: 'target'.
    Transcript show: '  Layer ', (target at: 'layer') asString, 
                     ' ', (target at: 'component').
].
```

### Attention Pattern Analysis

```smalltalk
"Comprehensive attention pattern analysis"

"Initialize attention analyzer"
attentionAnalyzer := AttentionAnalyzer new
    model: model;
    analysisLayers: #(6 8 10);
    patternTypes: #(#induction #copying #previousToken #syntactic);
    yourself.

"Define analysis texts with different linguistic phenomena"
analysisTexts := {
    {
        'text' -> 'John and Mary went to the store. John bought apples and Mary bought oranges.'.
        'phenomena' -> #('coreference' 'coordination').
        'description' -> 'Coreference and coordination'
    } asDictionary.
    {
        'text' -> 'The cat that sat on the mat was very comfortable.'.
        'phenomena' -> #('relative_clause' 'long_distance_dependency').
        'description' -> 'Relative clause with long-distance dependency'
    } asDictionary.
    {
        'text' -> 'When Alice arrived, she was tired because she had walked far.'.
        'phenomena' -> #('subordination' 'pronoun_resolution').
        'description' -> 'Complex subordination with pronouns'
    } asDictionary.
}.

"Analyze attention patterns for each text"
Transcript show: '=== Attention Pattern Analysis ==='.
analysisResults := OrderedCollection new.

analysisTexts do: [:example |
    text := example at: 'text'.
    phenomena := example at: 'phenomena'.
    description := example at: 'description'.
    
    Transcript show: 'Analyzing: ', description.
    Transcript show: '  Text: "', text, '"'.
    
    tokens := model tokenizer encode: text.
    tokenTexts := tokens collect: [:tokenId | model tokenizer decode: {tokenId}].
    
    "Extract attention weights from target layers"
    result := model runWithCaching: tokens 
                    layers: #(6 8 10) 
                    components: #(#attention #attentionWeights).
    
    layerResults := Dictionary new.
    
    #(6 8 10) do: [:layer |
        attentionWeights := result cachedActivations at: (layer -> #attentionWeights).
        
        "Analyze patterns in this layer"
        patterns := attentionAnalyzer analyzeLayer: layer 
                                      weights: attentionWeights 
                                      tokens: tokenTexts.
        
        layerResults at: layer put: patterns.
        
        Transcript show: '  Layer ', layer asString, ':'.
        patterns keysAndValuesDo: [:patternType :patternData |
            strength := patternData at: #strength.
            confidence := patternData at: #confidence.
            
            Transcript show: '    ', patternType, ': strength=', strength asString, 
                             ', confidence=', confidence asString.
            
            "Show specific pattern instances"
            instances := patternData at: #instances ifAbsent: [#()].
            instances first: (instances size min: 3) do: [:instance |
                fromToken := tokenTexts at: (instance at: #from) + 1.
                toToken := tokenTexts at: (instance at: #to) + 1.
                weight := instance at: #weight.
                
                Transcript show: '      "', fromToken, '" -> "', toToken, 
                                 '" (', weight asString, ')'.
            ].
        ].
    ].
    
    analysisResults add: {
        'example' -> example.
        'layerResults' -> layerResults.
    } asDictionary.
].

"Cross-layer pattern comparison"
Transcript show: '=== Cross-Layer Pattern Evolution ==='.
patternTypes := #(#induction #copying #previousToken #syntactic).

patternTypes do: [:patternType |
    Transcript show: 'Pattern: ', patternType.
    
    analysisResults do: [:result |
        example := result at: 'example'.
        layerResults := result at: 'layerResults'.
        description := example at: 'description'.
        
        Transcript show: '  ', description, ':'.
        
        #(6 8 10) do: [:layer |
            patterns := layerResults at: layer.
            patternData := patterns at: patternType ifAbsent: [
                {'strength' -> 0. 'confidence' -> 0} asDictionary
            ].
            
            strength := patternData at: #strength.
            Transcript show: '    Layer ', layer asString, ': ', strength asString.
        ].
    ].
].

"Generate attention visualizations"
Transcript show: '=== Generating Attention Visualizations ==='.
visualizer := AttentionVisualizer new.

analysisResults do: [:result |
    example := result at: 'example'.
    layerResults := result at: 'layerResults'.
    description := example at: 'description'.
    text := example at: 'text'.
    
    "Create visualization for each layer"
    #(6 8 10) do: [:layer |
        tokens := model tokenizer encode: text.
        attentionResult := model runWithCaching: tokens 
                                 layers: {layer} 
                                 components: #(#attentionWeights).
        
        attentionWeights := attentionResult cachedActivations at: (layer -> #attentionWeights).
        
        "Generate heatmap visualization"
        heatmap := visualizer createHeatmap: attentionWeights 
                                   tokens: (tokens collect: [:id | model tokenizer decode: {id}])
                                   layer: layer.
        
        filename := 'attention_layer_', layer asString, '_', 
                   (description copyReplaceAll: ' ' with: '_'), '.png'.
        heatmap saveAs: filename.
        
        Transcript show: 'Saved visualization: ', filename.
    ].
].
```

### End-to-End Research Workflow

```smalltalk
"Complete research workflow: from hypothesis to publication-ready results"

"Define research question"
researchQuestion := 'How do different layers contribute to indirect object identification in transformer models?'.
hypothesis := 'Middle layers (6-9) should show strongest patterns for indirect object identification, with attention mechanisms playing a key role.'.

Transcript show: '=== Research Workflow: Indirect Object Identification ==='.
Transcript show: 'Question: ', researchQuestion.
Transcript show: 'Hypothesis: ', hypothesis.

"Phase 1: Data Collection and Preparation"
Transcript show: '=== Phase 1: Data Collection ==='.

"Create comprehensive test dataset"
testDataset := {
    "Direct cases"
    {'John gave Mary the book' -> {'indirect_object' -> 'Mary'. 'position' -> 3}}.
    {'She handed him the keys' -> {'indirect_object' -> 'him'. 'position' -> 3}}.
    {'The teacher showed students the diagram' -> {'indirect_object' -> 'students'. 'position' -> 4}}.
    
    "Complex cases"
    {'The chef offered the hungry customer a special dish' -> {'indirect_object' -> 'the hungry customer'. 'position' -> 4}}.
    {'Mom read her children a bedtime story' -> {'indirect_object' -> 'her children'. 'position' -> 3}}.
    
    "Control cases (no indirect object)"
    {'John bought a book' -> {'indirect_object' -> nil. 'position' -> nil}}.
    {'She walked to the store' -> {'indirect_object' -> nil. 'position' -> nil}}.
    {'The cat sat on the mat' -> {'indirect_object' -> nil. 'position' -> nil}}.
} collect: [:pair | 
    {
        'sentence' -> pair key.
        'annotation' -> pair value.
    } asDictionary
].

Transcript show: 'Created dataset with ', testDataset size asString, ' examples'.

"Phase 2: Multi-Layer Activation Analysis"
Transcript show: '=== Phase 2: Activation Analysis ==='.

allLayerResults := Dictionary new.
targetLayers := 0 to: 11.

testDataset do: [:example |
    sentence := example at: 'sentence'.
    annotation := example at: 'annotation'.
    
    tokens := model tokenizer encode: sentence.
    
    "Extract activations from all layers"
    result := model runWithCaching: tokens 
                    layers: targetLayers 
                    components: #(#residual #attention #mlp).
    
    "Store results with metadata"
    exampleResults := {
        'sentence' -> sentence.
        'tokens' -> tokens.
        'annotation' -> annotation.
        'activations' -> result cachedActivations.
    } asDictionary.
    
    allLayerResults at: sentence put: exampleResults.
].

"Phase 3: Pattern Detection and Analysis"
Transcript show: '=== Phase 3: Pattern Detection ==='.

"Analyze attention patterns for indirect object detection"
indirectObjectPatterns := Dictionary new.

targetLayers do: [:layer |
    layerPatterns := OrderedCollection new.
    
    allLayerResults valuesDo: [:exampleResult |
        sentence := exampleResult at: 'sentence'.
        annotation := exampleResult at: 'annotation'.
        activations := exampleResult at: 'activations'.
        
        "Skip if no indirect object"
        (annotation at: 'indirect_object') ifNil: [^self].
        
        "Analyze attention patterns"
        attentionKey := layer -> #attention.
        (activations includesKey: attentionKey) ifTrue: [
            attention := activations at: attentionKey.
            
            "Look for attention to indirect object position"
            indirectObjectPos := annotation at: 'position'.
            
            "Calculate attention strength to indirect object"
            attentionToIO := self calculateAttentionToPosition: attention 
                                  position: indirectObjectPos.
            
            patternData := {
                'sentence' -> sentence.
                'layer' -> layer.
                'attention_strength' -> attentionToIO.
                'indirect_object' -> (annotation at: 'indirect_object').
            } asDictionary.
            
            layerPatterns add: patternData.
        ].
    ].
    
    indirectObjectPatterns at: layer put: layerPatterns.
].

"Phase 4: Statistical Analysis"
Transcript show: '=== Phase 4: Statistical Analysis ==='.

"Calculate layer-wise statistics"
layerStatistics := Dictionary new.

targetLayers do: [:layer |
    patterns := indirectObjectPatterns at: layer ifAbsent: [#()].
    
    patterns notEmpty ifTrue: [
        attentionStrengths := patterns collect: [:pattern | 
            pattern at: 'attention_strength'
        ].
        
        stats := {
            'layer' -> layer.
            'mean_attention' -> attentionStrengths mean.
            'std_attention' -> attentionStrengths std.
            'max_attention' -> attentionStrengths max.
            'sample_size' -> attentionStrengths size.
        } asDictionary.
        
        layerStatistics at: layer put: stats.
    ].
].

"Find peak layers for indirect object attention"
peakLayers := layerStatistics associations 
    asSortedCollection: [:a :b | 
        (a value at: 'mean_attention') > (b value at: 'mean_attention')
    ].

Transcript show: 'Layer ranking by indirect object attention:'.
peakLayers withIndexDo: [:assoc :rank |
    layer := assoc key.
    stats := assoc value.
    meanAttention := stats at: 'mean_attention'.
    
    Transcript show: rank asString, '. Layer ', layer asString, 
                     ': mean attention = ', meanAttention asString.
].

"Phase 5: Causal Validation"
Transcript show: '=== Phase 5: Causal Validation ==='.

"Test causal importance of top layers"
topLayers := peakLayers first: 3 collect: [:assoc | assoc key].

causalResults := Dictionary new.

topLayers do: [:layer |
    Transcript show: 'Testing causal importance of layer ', layer asString.
    
    "Create ablation hook"
    ablationHook := InterventionHook new
        name: 'layer_ablation';
        layer: layer;
        action: [:activation | activation * 0];  "Zero out layer"
        yourself.
    
    model hookManager addHook: ablationHook.
    
    "Test on indirect object sentences"
    ablationEffects := OrderedCollection new.
    
    testDataset do: [:example |
        sentence := example at: 'sentence'.
        annotation := example at: 'annotation'.
        
        "Skip control cases"
        (annotation at: 'indirect_object') ifNil: [^self].
        
        tokens := model tokenizer encode: sentence.
        
        "Get baseline prediction"
        model hookManager disableHook: 'layer_ablation'.
        baselineOutput := model forward: tokens.
        baselineLogits := baselineOutput logits.
        
        "Get ablated prediction"
        model hookManager enableHook: 'layer_ablation'.
        ablatedOutput := model forward: tokens.
        ablatedLogits := ablatedOutput logits.
        
        "Calculate effect"
        effect := (baselineLogits - ablatedLogits) abs mean.
        
        ablationEffects add: {
            'sentence' -> sentence.
            'effect' -> effect.
        } asDictionary.
    ].
    
    model hookManager removeHook: 'layer_ablation'.
    
    "Calculate average effect"
    avgEffect := (ablationEffects collect: [:result | result at: 'effect']) mean.
    
    causalResults at: layer put: {
        'average_effect' -> avgEffect.
        'individual_effects' -> ablationEffects.
    } asDictionary.
    
    Transcript show: '  Average ablation effect: ', avgEffect asString.
].

"Phase 6: Results Summary and Visualization"
Transcript show: '=== Phase 6: Results Summary ==='.

"Generate comprehensive report"
report := Dictionary new.
report at: 'research_question' put: researchQuestion.
report at: 'hypothesis' put: hypothesis.
report at: 'dataset_size' put: testDataset size.
report at: 'layer_statistics' put: layerStatistics.
report at: 'causal_results' put: causalResults.

"Key findings"
topLayer := peakLayers first key.
topLayerStats := layerStatistics at: topLayer.
topLayerCausal := causalResults at: topLayer.

Transcript show: '=== Key Findings ==='.
Transcript show: '1. Peak layer for indirect object attention: Layer ', topLayer asString.
Transcript show: '   Mean attention strength: ', (topLayerStats at: 'mean_attention') asString.
Transcript show: '   Causal importance (ablation effect): ', 
                 (topLayerCausal at: 'average_effect') asString.

"Hypothesis validation"
middleLayerRange := 6 to: 9.
topLayersInRange := peakLayers first: 4 collect: [:assoc | assoc key].
middleLayersInTop := topLayersInRange select: [:layer | 
    middleLayerRange includes: layer
].

hypothesisSupported := middleLayersInTop size >= 2.

Transcript show: '2. Hypothesis validation: ', 
                 (hypothesisSupported ifTrue: ['SUPPORTED'] ifFalse: ['NOT SUPPORTED']).
Transcript show: '   Middle layers in top 4: ', middleLayersInTop asString.

"Export results for publication"
reportExporter := ResearchReportExporter new.
reportExporter 
    exportReport: report 
    toFile: 'indirect_object_analysis_results.json'.

"Generate publication-quality figures"
figureGenerator := PublicationFigureGenerator new.

"Figure 1: Layer-wise attention strength"
attentionFigure := figureGenerator createBarChart: layerStatistics 
                                  title: 'Attention Strength to Indirect Objects by Layer'
                                  xLabel: 'Layer'
                                  yLabel: 'Mean Attention Strength'.
attentionFigure saveAs: 'figure1_attention_by_layer.svg'.

"Figure 2: Causal importance"
causalFigure := figureGenerator createBarChart: causalResults 
                               title: 'Causal Importance of Layers (Ablation Effects)'
                               xLabel: 'Layer'
                               yLabel: 'Average Ablation Effect'.
causalFigure saveAs: 'figure2_causal_importance.svg'.

"Figure 3: Example attention heatmaps"
exampleSentence := 'John gave Mary the book'.
exampleTokens := model tokenizer encode: exampleSentence.
exampleResult := model runWithCaching: exampleTokens 
                       layers: {topLayer} 
                       components: #(#attentionWeights).

attentionWeights := exampleResult cachedActivations at: (topLayer -> #attentionWeights).
heatmapFigure := figureGenerator createAttentionHeatmap: attentionWeights 
                                tokens: (exampleTokens collect: [:id | model tokenizer decode: {id}])
                                title: 'Attention Pattern for Indirect Object Detection'.
heatmapFigure saveAs: 'figure3_attention_heatmap.svg'.

Transcript show: '=== Research Workflow Complete ==='.
Transcript show: 'Results exported to: indirect_object_analysis_results.json'.
Transcript show: 'Figures saved: figure1_attention_by_layer.svg, figure2_causal_importance.svg, figure3_attention_heatmap.svg'.
```

### Browser Integration and Export

```smalltalk
"Demonstrate browser integration and data export capabilities"

"Initialize browser integration"
browserInterface := JSInterface new.
dataExporter := DataExporter new.

"Create interactive analysis dashboard"
Transcript show: '=== Creating Interactive Dashboard ==='.

"Set up real-time visualization"
dashboard := InteractiveLens new
    model: model;
    title: 'NeuroScope Analysis Dashboard';
    enableRealTimeUpdates: true;
    yourself.

"Add analysis panels"
dashboard addPanel: (AttentionHeatmapPanel new
    targetLayers: #(6 8 10);
    updateFrequency: #realtime;
    yourself).

dashboard addPanel: (ActivationStatisticsPanel new
    components: #(#residual #attention #mlp);
    statisticsTypes: #(#mean #std #max #activationRate);
    yourself).

dashboard addPanel: (InterventionControlPanel new
    availableInterventions: #(#attentionAblation #activationScaling #layerDropout);
    yourself).

"Launch dashboard in browser"
dashboardURL := dashboard launchInBrowser.
Transcript show: 'Dashboard launched at: ', dashboardURL.

"Set up data export functionality"
Transcript show: '=== Setting Up Data Export ==='.

"Export activation data to various formats"
analysisText := 'The cat sat on the mat and looked around curiously.'.
tokens := model tokenizer encode: analysisText.

"Comprehensive data extraction"
exportData := model runWithCaching: tokens 
                    layers: (0 to: 11) 
                    components: #(#residual #attention #mlp #layerNorm).

"Export to JSON for web applications"
jsonExporter := JSONExporter new.
jsonData := jsonExporter export: exportData cachedActivations 
                         metadata: {
                             'text' -> analysisText.
                             'tokens' -> tokens.
                             'model' -> 'gpt2-small'.
                             'timestamp' -> DateAndTime now asString.
                         } asDictionary.

browserInterface saveToLocalStorage: 'neuroscope_analysis_data' data: jsonData.
Transcript show: 'Data exported to browser local storage'.

"Export to CSV for statistical analysis"
csvExporter := CSVExporter new.
csvData := csvExporter exportActivationStatistics: exportData cachedActivations.
browserInterface downloadFile: 'activation_statistics.csv' content: csvData.
Transcript show: 'CSV file prepared for download'.

"Export visualizations as interactive HTML"
htmlExporter := HTMLExporter new.
interactiveVisualization := htmlExporter createInteractiveVisualization: exportData 
                                       title: 'NeuroScope Analysis Results'
                                       includeControls: true.

browserInterface openInNewTab: interactiveVisualization.
Transcript show: 'Interactive visualization opened in new tab'.

"Set up real-time collaboration features"
Transcript show: '=== Real-time Collaboration Setup ==='.

"Enable sharing of analysis sessions"
collaborationManager := CollaborationManager new.
sessionId := collaborationManager createSession: 'neuroscope_analysis_session'.

"Share session with collaborators"
shareableLink := collaborationManager generateShareableLink: sessionId.
Transcript show: 'Shareable analysis session: ', shareableLink.

"Set up real-time synchronization"
collaborationManager onRemoteUpdate: [:update |
    "Handle updates from other users"
    updateType := update at: 'type'.
    updateData := update at: 'data'.
    
    updateType = 'intervention' ifTrue: [
        "Apply intervention from remote user"
        interventionConfig := updateData at: 'config'.
        self applyRemoteIntervention: interventionConfig.
        dashboard refreshVisualization.
    ].
    
    updateType = 'analysis' ifTrue: [
        "Display analysis from remote user"
        analysisResults := updateData at: 'results'.
        dashboard displayRemoteAnalysis: analysisResults.
    ].
].

"Enable persistent storage and retrieval"
Transcript show: '=== Persistent Storage Setup ==='.

"Save analysis state to IndexedDB"
storageManager := BrowserStorage new.
analysisState := {
    'model_config' -> model config.
    'current_text' -> analysisText.
    'cached_activations' -> exportData cachedActivations.
    'dashboard_config' -> dashboard getConfiguration.
    'timestamp' -> DateAndTime now asString.
} asDictionary.

storageManager saveAnalysisState: 'current_session' state: analysisState.
Transcript show: 'Analysis state saved to persistent storage'.

"Set up automatic backup"
backupManager := BackupManager new.
backupManager enableAutoBackup: true interval: 300000.  "5 minutes"
backupManager onBackupComplete: [:backupId |
    Transcript show: 'Analysis backed up with ID: ', backupId.
].

"Create export package for offline analysis"
Transcript show: '=== Creating Export Package ==='.

exportPackage := ExportPackage new
    includeModel: false;  "Don't include model weights"
    includeActivations: true;
    includeVisualizations: true;
    includeCode: true;
    format: #zip;
    yourself.

packageData := exportPackage createPackage: analysisState.
browserInterface downloadFile: 'neuroscope_analysis_package.zip' content: packageData.

Transcript show: '=== Browser Integration Complete ==='.
Transcript show: 'All analysis results are now available through the browser interface'.
Transcript show: 'Dashboard URL: ', dashboardURL.
Transcript show: 'Collaboration session: ', shareableLink.
Transcript show: 'Export package ready for download'.
```

This comprehensive set of advanced analysis workflow examples demonstrates sophisticated interpretability research patterns, including automated circuit discovery, probe training, systematic intervention analysis, attention pattern analysis, complete research workflows, and browser integration capabilities. These examples provide researchers with practical templates for conducting cutting-edge mechanistic interpretability research using the NeuroScope framework.