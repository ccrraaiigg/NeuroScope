# ActivationHook Class Documentation

## Purpose

ActivationHook specializes the base Hook class for non-invasive monitoring and extraction of neural network activations during forward passes. It provides sophisticated caching, filtering, and analysis capabilities while ensuring zero impact on model computation, making it the primary tool for observational interpretability research.

## Responsibilities

- **Activation Extraction**: Capture activations at specific layers and components without modifying computation flow
- **Intelligent Caching**: Implement efficient caching strategies with memory management and selective storage
- **Data Filtering**: Apply filters and transformations to extracted activations for focused analysis
- **Temporal Tracking**: Track activation patterns across multiple forward passes and sequences
- **Analysis Integration**: Provide seamless integration with analysis tools and visualization components
- **Performance Optimization**: Minimize computational and memory overhead during extraction
- **Result Management**: Organize and provide access to extracted activation data with rich metadata

## Key Concepts

ActivationHook operates as a passive observer in the neural network computation graph, extracting activation tensors at predetermined points without altering the forward pass. It implements sophisticated caching mechanisms that balance memory usage with analysis needs, supporting both real-time analysis and batch processing workflows.

The hook supports multiple extraction modes: **immediate analysis** (process activations as they're generated), **deferred analysis** (cache for later processing), and **streaming analysis** (process activations in chunks for memory efficiency). This flexibility enables everything from lightweight monitoring to comprehensive activation dataset collection.

Integration with the forward pass execution is designed to be completely transparent, with activation extraction occurring through efficient tensor copying and metadata preservation that maintains full compatibility with gradient computation and model optimization.

## Instance Variables

- **extractionTargets**: Specification of which activations to extract, supporting layer indices, component names (attention, mlp, residual), head indices for attention layers, and neuron ranges for MLP layers. Can be dynamically updated during execution.

- **cacheStrategy**: Caching configuration including memory limits, eviction policies (LRU, FIFO, priority-based), compression settings, and persistence options. Balances memory usage with analysis requirements.

- **filterCriteria**: Optional filtering conditions applied to activations before caching, including value thresholds, statistical criteria (top-k, percentile-based), spatial filters (specific token positions), and temporal filters (sequence ranges).

- **analysisCallbacks**: Collection of analysis functions that process activations immediately upon extraction. Enables real-time analysis without caching overhead for simple monitoring tasks.

- **extractedData**: Organized storage of cached activations with rich metadata including timestamps, model state, input context, and extraction parameters. Supports efficient querying and batch processing.

- **compressionSettings**: Configuration for activation data compression including compression algorithms, quality settings, and selective compression based on activation characteristics. Reduces memory footprint for large-scale studies.

## Usage Patterns

### Basic Activation Extraction
```smalltalk
"Simple layer monitoring"
layerMonitor := ActivationHook new
    name: 'layer_5_monitor';
    layer: 5;
    extractionTargets: #(residual attention mlp);
    cacheStrategy: (CacheStrategy memory: 100 megabytes);
    yourself.

model hookManager addHook: layerMonitor.

"Execute forward pass"
tokens := model tokenizer encode: 'The cat sat on the mat'.
output := model forward: tokens.

"Access extracted activations"
residualActivations := layerMonitor extractedData at: #residual.
attentionActivations := layerMonitor extractedData at: #attention.
mlpActivations := layerMonitor extractedData at: #mlp.

"Analyze extracted data"
residualStats := residualActivations computeStatistics.
attentionPatterns := attentionActivations extractPatterns.
```

### Advanced Caching and Filtering
```smalltalk
"Sophisticated activation extraction with filtering"
selectiveExtractor := ActivationHook new
    name: 'selective_activation_extractor';
    layer: #(5 8 11);
    extractionTargets: #(attention);
    filterCriteria: (FilterCriteria new
        topKActivations: 100;
        minimumMagnitude: 0.1;
        tokenPositions: (1 to: 20);
        yourself);
    cacheStrategy: (CacheStrategy new
        memoryLimit: 500 megabytes;
        evictionPolicy: #LRU;
        compressionEnabled: true;
        compressionQuality: 0.8;
        persistToDisk: true;
        diskPath: 'activations/selective_cache';
        yourself);
    yourself.

"Add real-time analysis"
selectiveExtractor addAnalysisCallback: [:activation :metadata |
    | patterns |
    patterns := AttentionPatternAnalyzer analyze: activation.
    patterns isInteresting ifTrue: [
        self flagForDetailedAnalysis: metadata.
    ].
].

model hookManager addHook: selectiveExtractor.

"Process multiple sequences"
testSequences := self loadTestSequences.
testSequences do: [:sequence |
    tokens := model tokenizer encode: sequence.
    output := model forward: tokens.
].

"Access filtered and cached results"
interestingActivations := selectiveExtractor extractedData 
    select: [:data | data metadata isFlagged].
compressionStats := selectiveExtractor cacheStrategy compressionStatistics.
```

### Temporal Analysis and Sequence Tracking
```smalltalk
"Track activation evolution across sequences"
temporalTracker := ActivationHook new
    name: 'temporal_activation_tracker';
    layer: 8;
    extractionTargets: #(attention);
    enableTemporalTracking: true;
    temporalWindow: 50; "Track last 50 forward passes"
    yourself.

"Add temporal analysis callbacks"
temporalTracker 
    onSequenceStart: [:metadata | 
        self initializeSequenceTracking: metadata];
    onSequenceEnd: [:metadata | 
        self finalizeSequenceAnalysis: metadata];
    onActivationExtracted: [:activation :position :metadata |
        self updateTemporalModel: activation at: position with: metadata].

model hookManager addHook: temporalTracker.

"Process conversation or document"
conversation := self loadConversation.
conversation turns do: [:turn |
    tokens := model tokenizer encode: turn text.
    response := model forward: tokens.
    
    "Analyze temporal patterns"
    currentPatterns := temporalTracker currentTemporalPatterns.
    evolutionMetrics := temporalTracker computeEvolutionMetrics.
].

"Generate temporal analysis report"
temporalReport := temporalTracker generateTemporalReport.
evolutionGraphs := temporalTracker visualizeEvolution.
```

## Integration Points

### With Forward Pass Execution
ActivationHook integrates seamlessly with the model's forward pass, receiving activation tensors at precise computation points without interfering with gradient flow or model optimization.

### With ActivationTensor
The hook operates directly on ActivationTensor instances, preserving all metadata, device placement, and computational graph information while extracting data for analysis.

### With Analysis Tools
Extracted activations are formatted for direct use with analysis tools like AttentionAnalyzer, NeuronAnalyzer, and CircuitFinder, enabling seamless integration with interpretability workflows.

### With Visualization Components
Cached activations can be directly fed to visualization components, supporting real-time visualization and interactive exploration of neural network behavior.

### With Storage Systems
The hook integrates with browser storage systems (IndexedDB, localStorage) and supports data export for external analysis tools and research workflows.

## Examples

### Comprehensive Activation Dataset Collection
```smalltalk
"Create comprehensive dataset collection setup"
datasetCollector := ActivationHook new
    name: 'comprehensive_dataset_collector';
    layer: #all;
    extractionTargets: #(residual attention mlp embedding);
    cacheStrategy: (CacheStrategy new
        memoryLimit: 2 gigabytes;
        evictionPolicy: #priority;
        compressionEnabled: true;
        compressionAlgorithm: #lz4;
        persistToDisk: true;
        diskPath: 'datasets/comprehensive_activations';
        batchSize: 1000;
        yourself);
    filterCriteria: (FilterCriteria new
        sampleRate: 0.1; "Sample 10% of activations"
        diversityFilter: true; "Ensure diverse samples"
        qualityThreshold: 0.05; "Minimum activation magnitude"
        yourself);
    yourself.

"Add metadata enrichment"
datasetCollector enrichMetadata: [:activation :context |
    Dictionary new
        at: #inputText put: context inputText;
        at: #tokenPosition put: context tokenPosition;
        at: #layerIndex put: context layerIndex;
        at: #componentType put: context componentType;
        at: #timestamp put: DateAndTime now;
        at: #modelState put: context modelState;
        at: #activationStats put: activation computeBasicStatistics;
        yourself
].

model hookManager addHook: datasetCollector.

"Process large corpus"
corpus := self loadLargeCorpus. "10,000 documents"
progressBar := ProgressBar new maxValue: corpus size.

corpus withIndexDo: [:document :index |
    tokens := model tokenizer encode: document text.
    output := model forward: tokens.
    
    progressBar current: index.
    
    "Periodic dataset validation"
    (index \\ 1000) = 0 ifTrue: [
        datasetCollector validateDatasetIntegrity.
        self reportProgress: index total: corpus size.
    ].
].

"Generate dataset summary"
datasetSummary := datasetCollector generateDatasetSummary.
qualityMetrics := datasetCollector computeQualityMetrics.
exportPath := datasetCollector exportDataset: 'final_activation_dataset.h5'.
```

### Real-time Analysis and Monitoring
```smalltalk
"Real-time activation monitoring system"
realtimeMonitor := ActivationHook new
    name: 'realtime_monitor';
    layer: #(5 8 11);
    extractionTargets: #(attention);
    cacheStrategy: (CacheStrategy new
        memoryLimit: 50 megabytes;
        evictionPolicy: #FIFO;
        maxAge: 60 seconds;
        yourself);
    yourself.

"Add real-time analysis callbacks"
realtimeMonitor
    addAnalysisCallback: [:activation :metadata |
        "Detect anomalous patterns"
        anomalyScore := AnomalyDetector score: activation.
        anomalyScore > 0.8 ifTrue: [
            self alertAnomalousActivation: activation with: metadata.
        ].
        
        "Update live statistics"
        self updateLiveStatistics: activation.
        
        "Trigger visualization updates"
        self updateVisualization: activation.
    ];
    
    addPerformanceCallback: [:executionTime :memoryUsage |
        "Monitor hook performance"
        executionTime > 10 milliseconds ifTrue: [
            self optimizeExtractionStrategy.
        ].
        
        memoryUsage > 40 megabytes ifTrue: [
            self triggerCacheCleanup.
        ].
    ].

"Start real-time monitoring"
model hookManager addHook: realtimeMonitor.

"Interactive analysis session"
interactiveSession := InteractiveAnalysisSession new
    model: model;
    monitor: realtimeMonitor;
    yourself.

interactiveSession start.

"Process user inputs in real-time"
[interactiveSession isActive] whileTrue: [
    userInput := self getUserInput.
    tokens := model tokenizer encode: userInput.
    response := model forward: tokens.
    
    "Real-time analysis results available immediately"
    currentPatterns := realtimeMonitor currentAnalysisResults.
    anomalies := realtimeMonitor detectedAnomalies.
    
    self displayResults: response patterns: currentPatterns anomalies: anomalies.
    
    "Update interactive visualizations"
    interactiveSession updateVisualizations.
].
```

### Specialized Extraction Patterns
```smalltalk
"Attention head-specific extraction"
headSpecificExtractor := ActivationHook new
    name: 'attention_head_extractor';
    layer: 8;
    extractionTargets: (AttentionTargets new
        heads: #(3 7 11); "Specific attention heads"
        queryKeyValue: #all; "Extract Q, K, V matrices"
        attentionWeights: true; "Include attention weights"
        yourself);
    cacheStrategy: (CacheStrategy new
        memoryLimit: 200 megabytes;
        organizationStrategy: #byHead; "Organize by attention head"
        yourself);
    yourself.

"Add head-specific analysis"
headSpecificExtractor addAnalysisCallback: [:activation :metadata |
    headIndex := metadata headIndex.
    
    "Analyze head-specific patterns"
    headPatterns := self analyzeAttentionHead: headIndex activation: activation.
    
    "Track head specialization"
    self updateHeadSpecialization: headIndex patterns: headPatterns.
    
    "Detect head interactions"
    self detectHeadInteractions: headIndex with: activation.
].

"Neuron-level MLP extraction"
neuronExtractor := ActivationHook new
    name: 'neuron_level_extractor';
    layer: 6;
    extractionTargets: (MLPTargets new
        neurons: (1 to: 100); "First 100 neurons"
        activationFunction: #gelu;
        preActivation: true; "Before activation function"
        postActivation: true; "After activation function"
        yourself);
    filterCriteria: (FilterCriteria new
        topKNeurons: 20; "Most active neurons"
        activationThreshold: 0.5;
        yourself);
    yourself.

"Token-position specific extraction"
positionalExtractor := ActivationHook new
    name: 'positional_extractor';
    layer: #all;
    extractionTargets: #(residual);
    filterCriteria: (FilterCriteria new
        tokenPositions: #(1 -1); "First and last tokens"
        sequenceLength: (10 to: 50); "Medium-length sequences"
        yourself);
    cacheStrategy: (CacheStrategy new
        organizationStrategy: #byPosition;
        yourself);
    yourself.

"Register specialized extractors"
model hookManager 
    addHook: headSpecificExtractor;
    addHook: neuronExtractor;
    addHook: positionalExtractor.

"Execute specialized analysis"
specializedTexts := self loadSpecializedTestTexts.
specializedTexts do: [:text |
    tokens := model tokenizer encode: text.
    output := model forward: tokens.
].

"Access specialized results"
headAnalysis := headSpecificExtractor generateHeadAnalysisReport.
neuronCharacterization := neuronExtractor characterizeNeurons.
positionalPatterns := positionalExtractor analyzePositionalPatterns.
```

### Integration with Analysis Workflows
```smalltalk
"Circuit discovery workflow integration"
circuitDiscoveryExtractor := ActivationHook new
    name: 'circuit_discovery_extractor';
    layer: #(5 8 11);
    extractionTargets: #(attention mlp residual);
    cacheStrategy: (CacheStrategy new
        memoryLimit: 1 gigabyte;
        organizationStrategy: #byCircuit;
        enableCrossLayerAnalysis: true;
        yourself);
    yourself.

"Add circuit analysis callbacks"
circuitDiscoveryExtractor
    addAnalysisCallback: [:activation :metadata |
        "Identify potential circuit components"
        components := CircuitComponentDetector detect: activation.
        components do: [:component |
            self registerCircuitComponent: component at: metadata layer.
        ].
    ];
    
    onExtractionComplete: [
        "Perform cross-layer circuit analysis"
        circuits := CircuitFinder findCircuits: self extractedData.
        self validateCircuits: circuits.
    ].

"Probe training data collection"
probeDataCollector := ActivationHook new
    name: 'probe_training_data';
    layer: 8;
    extractionTargets: #(residual);
    cacheStrategy: (CacheStrategy new
        memoryLimit: 500 megabytes;
        organizationStrategy: #byLabel;
        enableLabeling: true;
        yourself);
    yourself.

"Add automatic labeling"
probeDataCollector addLabelingCallback: [:activation :metadata |
    | label |
    label := self extractGroundTruthLabel: metadata inputText.
    activation addLabel: label.
    activation
].

"Intervention effect measurement"
interventionMeasurer := ActivationHook new
    name: 'intervention_effect_measurer';
    layer: #all;
    extractionTargets: #(residual);
    enableBaselineComparison: true;
    cacheStrategy: (CacheStrategy new
        memoryLimit: 300 megabytes;
        organizationStrategy: #byIntervention;
        yourself);
    yourself.

"Coordinate complex analysis workflow"
analysisWorkflow := ComplexAnalysisWorkflow new
    addExtractor: circuitDiscoveryExtractor;
    addExtractor: probeDataCollector;
    addExtractor: interventionMeasurer;
    yourself.

model hookManager addHooks: analysisWorkflow extractors.

"Execute comprehensive analysis"
analysisResults := analysisWorkflow execute: testDataset.

"Generate integrated report"
comprehensiveReport := analysisWorkflow generateIntegratedReport: analysisResults.
```

This comprehensive documentation establishes ActivationHook as the primary tool for non-invasive activation extraction and monitoring in NeuroScope, providing researchers with sophisticated capabilities for observational interpretability studies while maintaining optimal performance and integration with the broader framework.