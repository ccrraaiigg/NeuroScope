# ProbeHook Class Documentation

## Purpose

ProbeHook specializes the base Hook class for training and deploying linear probes that extract interpretable information from neural network activations. It provides comprehensive probe training capabilities, automatic data collection, and integration with machine learning frameworks while maintaining efficient execution during forward passes.

## Responsibilities

- **Probe Training**: Implement supervised learning for training linear probes on activation data with various target labels
- **Data Collection**: Automatically collect and organize training data from model activations with proper labeling
- **Feature Engineering**: Apply feature transformations and selection techniques to improve probe performance
- **Model Management**: Handle probe model lifecycle including training, validation, deployment, and updating
- **Performance Monitoring**: Track probe accuracy, generalization, and interpretability metrics over time
- **Integration Support**: Seamlessly integrate with existing analysis workflows and visualization tools
- **Memory Optimization**: Efficiently manage probe models and training data to minimize memory footprint

## Key Concepts

ProbeHook operates by extracting activations during forward passes and using them to train lightweight linear models that predict specific properties or behaviors. This enables researchers to understand what information is encoded in different parts of the network by testing whether simple linear classifiers can extract that information.

The hook supports multiple probe types: **classification probes** (predicting discrete labels), **regression probes** (predicting continuous values), **multi-task probes** (predicting multiple targets simultaneously), and **temporal probes** (predicting based on activation sequences). Each probe type uses appropriate loss functions and evaluation metrics.

Critical to the design is the balance between probe complexity and interpretability. Linear probes provide clear interpretability but may miss complex patterns, while slightly more complex probes (e.g., with feature interactions) can capture richer patterns while maintaining reasonable interpretability.

## Instance Variables

- **probeType**: Specifies the type of probe including #classification, #regression, #multiTask, #temporal, and #contrastive. Determines the learning algorithm, loss function, and evaluation metrics used for training.

- **targetLabels**: Configuration for target labels including label extraction functions, label preprocessing, multi-class handling, and temporal alignment. Supports both manual labeling and automatic label extraction from input context.

- **trainingConfiguration**: Training parameters including learning rate, regularization strength, batch size, number of epochs, early stopping criteria, and cross-validation settings. Optimized for linear probe training characteristics.

- **featureProcessing**: Feature engineering configuration including normalization methods, dimensionality reduction, feature selection criteria, and activation preprocessing. Balances probe performance with interpretability.

- **probeModel**: The trained probe model including learned parameters, training history, validation metrics, and deployment configuration. Supports model serialization and incremental updates.

- **dataManagement**: Configuration for training data collection including sampling strategies, data augmentation, memory limits, and storage options. Handles large-scale data collection efficiently.

## Usage Patterns

### Basic Classification Probe
```smalltalk
"Simple sentiment classification probe"
sentimentProbe := ProbeHook new
    name: 'sentiment_probe';
    layer: 8;
    probeType: #classification;
    targetLabels: (ClassificationLabels new
        labelExtractor: [:inputText | self extractSentiment: inputText];
        classes: #(positive negative neutral);
        yourself);
    trainingConfiguration: (TrainingConfig new
        learningRate: 0.01;
        regularization: 0.001;
        maxEpochs: 100;
        earlyStoppingPatience: 10;
        yourself);
    featureProcessing: (FeatureProcessing new
        normalization: #standardize;
        dimensionalityReduction: nil;
        yourself);
    yourself.

model hookManager addHook: sentimentProbe.

"Collect training data"
trainingTexts := self loadSentimentDataset.
trainingTexts do: [:text |
    tokens := model tokenizer encode: text.
    output := model forward: tokens.
].

"Train the probe"
trainingResults := sentimentProbe trainProbe.
accuracy := trainingResults accuracy.
confusionMatrix := trainingResults confusionMatrix.

"Deploy trained probe"
sentimentProbe deployProbe.

"Use probe for prediction"
testText := 'This movie is fantastic!'.
testTokens := model tokenizer encode: testText.
testOutput := model forward: testTokens.
sentimentPrediction := sentimentProbe lastPrediction.
```

### Multi-task Probe Training
```smalltalk
"Multi-task probe for linguistic features"
linguisticProbe := ProbeHook new
    name: 'linguistic_features_probe';
    layer: 6;
    probeType: #multiTask;
    targetLabels: (MultiTaskLabels new
        addTask: #partOfSpeech extractor: [:token | self extractPOS: token];
        addTask: #syntacticRole extractor: [:token | self extractSyntax: token];
        addTask: #semanticCategory extractor: [:token | self extractSemantics: token];
        yourself);
    trainingConfiguration: (MultiTaskTrainingConfig new
        sharedLearningRate: 0.01;
        taskSpecificRates: (Dictionary new
            at: #partOfSpeech put: 0.015;
            at: #syntacticRole put: 0.008;
            at: #semanticCategory put: 0.012;
            yourself);
        regularization: 0.001;
        taskWeights: (Dictionary new
            at: #partOfSpeech put: 1.0;
            at: #syntacticRole put: 0.8;
            at: #semanticCategory put: 1.2;
            yourself);
        yourself);
    yourself.

"Advanced feature processing for linguistic tasks"
linguisticProbe featureProcessing: (FeatureProcessing new
    normalization: #layerNorm;
    featureSelection: (FeatureSelection new
        method: #mutualInformation;
        topK: 500;
        yourself);
    contextualFeatures: true;
    positionalEncoding: true;
    yourself).

"Collect diverse linguistic data"
linguisticDataset := self loadLinguisticDataset.
linguisticDataset do: [:sentence |
    tokens := model tokenizer encode: sentence text.
    output := model forward: tokens.
].

"Train multi-task probe"
multiTaskResults := linguisticProbe trainProbe.

"Analyze task-specific performance"
posAccuracy := multiTaskResults taskAccuracy at: #partOfSpeech.
syntaxAccuracy := multiTaskResults taskAccuracy at: #syntacticRole.
semanticsAccuracy := multiTaskResults taskAccuracy at: #semanticCategory.

"Analyze feature importance across tasks"
featureImportance := linguisticProbe analyzeFeatureImportance.
sharedFeatures := featureImportance findSharedFeatures.
taskSpecificFeatures := featureImportance findTaskSpecificFeatures.
```

### Temporal Sequence Probing
```smalltalk
"Temporal probe for sequence prediction"
temporalProbe := ProbeHook new
    name: 'temporal_sequence_probe';
    layer: 10;
    probeType: #temporal;
    targetLabels: (TemporalLabels new
        sequenceLength: 5;
        predictionHorizon: 3;
        labelExtractor: [:sequence | self extractNextTokens: sequence];
        yourself);
    trainingConfiguration: (TemporalTrainingConfig new
        learningRate: 0.005;
        sequenceBatchSize: 32;
        temporalRegularization: 0.01;
        recurrentConnections: false; "Keep linear for interpretability"
        yourself);
    featureProcessing: (TemporalFeatureProcessing new
        temporalNormalization: #sequenceStandardize;
        temporalAggregation: #concatenate;
        positionEncoding: true;
        yourself);
    yourself.

"Collect temporal training data"
temporalDataset := self loadTemporalDataset.
temporalDataset do: [:sequence |
    sequence tokens do: [:tokenSubset |
        tokens := model tokenizer encode: tokenSubset.
        output := model forward: tokens.
    ].
].

"Train temporal probe"
temporalResults := temporalProbe trainProbe.
temporalAccuracy := temporalResults temporalAccuracy.
predictionHorizonAnalysis := temporalResults analyzeByHorizon.

"Analyze temporal patterns"
temporalPatterns := temporalProbe extractTemporalPatterns.
sequenceDependencies := temporalProbe analyzeSequenceDependencies.
```

## Integration Points

### With Machine Learning Frameworks
ProbeHook integrates with standard ML libraries for training, supporting scikit-learn, PyTorch, and TensorFlow backends through JavaScript bridges while maintaining Smalltalk-native interfaces.

### With Data Collection Systems
The hook coordinates with ActivationHook and other data collection mechanisms to efficiently gather training data without duplication or excessive memory usage.

### With Analysis Workflows
Trained probes integrate seamlessly with analysis tools, providing interpretable predictions that can be used for circuit discovery, attention analysis, and causal studies.

### With Visualization Tools
Probe results and feature importance visualizations integrate with the broader NeuroScope visualization system for interactive exploration of learned representations.

### With Model Management
The hook supports model versioning, A/B testing of different probe configurations, and automated retraining based on performance degradation detection.

## Examples

### Comprehensive Probe Training Pipeline
```smalltalk
"Complete probe training and analysis pipeline"
comprehensiveProbe := ProbeHook new
    name: 'comprehensive_analysis_probe';
    layer: 8;
    probeType: #classification;
    targetLabels: (ClassificationLabels new
        labelExtractor: [:context | self extractComplexLabel: context];
        classes: #(factual opinion question command);
        balanceClasses: true;
        yourself);
    trainingConfiguration: (TrainingConfig new
        learningRate: 0.01;
        regularization: 0.001;
        crossValidationFolds: 5;
        hyperparameterSearch: true;
        searchSpace: (HyperparameterSpace new
            learningRate: (0.001 to: 0.1 by: 0.001);
            regularization: (0.0001 to: 0.01 by: 0.0001);
            yourself);
        yourself);
    featureProcessing: (FeatureProcessing new
        normalization: #standardize;
        featureSelection: (FeatureSelection new
            method: #recursiveFeatureElimination;
            targetFeatures: 200;
            yourself);
        yourself);
    dataManagement: (DataManagement new
        samplingStrategy: #stratified;
        maxSamples: 10000;
        validationSplit: 0.2;
        testSplit: 0.1;
        yourself);
    yourself.

"Data collection with quality control"
comprehensiveProbe enableQualityControl: (QualityControl new
    minActivationMagnitude: 0.01;
    maxActivationMagnitude: 100.0;
    outlierDetection: true;
    duplicateRemoval: true;
    yourself).

model hookManager addHook: comprehensiveProbe.

"Collect high-quality training data"
qualityDataset := self loadHighQualityDataset.
progressBar := ProgressBar new maxValue: qualityDataset size.

qualityDataset withIndexDo: [:sample :index |
    tokens := model tokenizer encode: sample text.
    output := model forward: tokens.
    
    progressBar current: index.
    
    "Periodic quality assessment"
    (index \\ 1000) = 0 ifTrue: [
        qualityMetrics := comprehensiveProbe assessDataQuality.
        self reportDataQuality: qualityMetrics.
    ].
].

"Comprehensive training with hyperparameter optimization"
trainingResults := comprehensiveProbe trainProbeWithOptimization.

"Analyze training results"
bestHyperparameters := trainingResults bestHyperparameters.
crossValidationScores := trainingResults crossValidationScores.
featureImportance := trainingResults featureImportance.
learningCurves := trainingResults learningCurves.

"Deploy optimized probe"
comprehensiveProbe deployOptimizedProbe: bestHyperparameters.

"Comprehensive evaluation"
evaluationResults := comprehensiveProbe comprehensiveEvaluation: (EvaluationConfig new
    testMetrics: #(accuracy precision recall f1Score);
    interpretabilityAnalysis: true;
    robustnessTests: true;
    biasAnalysis: true;
    yourself).

"Generate detailed report"
probeReport := ProbeAnalysisReport new
    trainingResults: trainingResults;
    evaluationResults: evaluationResults;
    featureAnalysis: featureImportance;
    generateComprehensiveReport.
```

### Probe Ensemble and Comparison
```smalltalk
"Create ensemble of probes for robust analysis"
probeEnsemble := ProbeEnsemble new
    name: 'interpretability_ensemble';
    yourself.

"Add diverse probe configurations"
layers := #(5 8 11).
probeTypes := #(classification regression).
features := #(raw normalized pca).

layers do: [:layer |
    probeTypes do: [:type |
        features do: [:feature |
            probe := ProbeHook new
                name: 'probe_', layer asString, '_', type asString, '_', feature asString;
                layer: layer;
                probeType: type;
                featureProcessing: (self createFeatureProcessing: feature);
                yourself.
            
            probeEnsemble addProbe: probe.
        ].
    ].
].

"Register ensemble"
model hookManager addHooks: probeEnsemble probes.

"Collect data for all probes"
ensembleDataset := self loadEnsembleDataset.
ensembleDataset do: [:sample |
    tokens := model tokenizer encode: sample text.
    output := model forward: tokens.
].

"Train all probes in parallel"
ensembleResults := probeEnsemble trainAllProbes.

"Compare probe performance"
performanceComparison := probeEnsemble compareProbePerformance.
bestProbes := performanceComparison topProbes: 5.
layerComparison := performanceComparison compareByLayer.
typeComparison := performanceComparison compareByType.
featureComparison := performanceComparison compareByFeature.

"Ensemble prediction"
ensemblePredictor := probeEnsemble createEnsemblePredictor: (EnsembleConfig new
    votingStrategy: #weightedVoting;
    weights: performanceComparison accuracyWeights;
    confidenceThreshold: 0.8;
    yourself).

"Test ensemble prediction"
testSample := 'This is a test sentence for ensemble prediction.'.
testTokens := model tokenizer encode: testSample.
testOutput := model forward: testTokens.

ensemblePrediction := ensemblePredictor predict.
predictionConfidence := ensemblePredictor lastConfidence.
individualPredictions := ensemblePredictor individualPredictions.
```

### Advanced Probe Analysis
```smalltalk
"Probe interpretability analysis"
interpretabilityAnalyzer := ProbeInterpretabilityAnalyzer new
    probe: comprehensiveProbe;
    analysisDepth: #comprehensive;
    yourself.

"Feature importance analysis"
featureAnalysis := interpretabilityAnalyzer analyzeFeatureImportance: (FeatureAnalysisConfig new
    permutationImportance: true;
    shapValues: true;
    featureInteractions: true;
    topKFeatures: 50;
    yourself).

"Activation pattern analysis"
activationAnalysis := interpretabilityAnalyzer analyzeActivationPatterns: (ActivationAnalysisConfig new
    clusterAnalysis: true;
    dimensionalityReduction: #tsne;
    patternVisualization: true;
    yourself).

"Probe robustness testing"
robustnessAnalysis := interpretabilityAnalyzer testRobustness: (RobustnessConfig new
    noiseTests: true;
    adversarialTests: true;
    distributionShiftTests: true;
    yourself).

"Generate interpretability report"
interpretabilityReport := interpretabilityAnalyzer generateReport: (ReportConfig new
    includeVisualizations: true;
    includeStatistics: true;
    includeRecommendations: true;
    yourself).

"Probe-based circuit discovery"
circuitDiscovery := ProbeBasedCircuitDiscovery new
    probe: comprehensiveProbe;
    model: model;
    yourself.

discoveredCircuits := circuitDiscovery discoverCircuits: (CircuitDiscoveryConfig new
    significanceThreshold: 0.01;
    minimumCircuitSize: 3;
    maximumCircuitSize: 20;
    validationMethod: #crossValidation;
    yourself).

"Validate discovered circuits"
circuitValidation := circuitDiscovery validateCircuits: discoveredCircuits.
validatedCircuits := circuitValidation validCircuits.
circuitConfidence := circuitValidation confidenceScores.
```

This comprehensive documentation establishes ProbeHook as a sophisticated tool for training and deploying linear probes in NeuroScope, enabling researchers to extract interpretable information from neural network representations while maintaining integration with the broader interpretability framework.