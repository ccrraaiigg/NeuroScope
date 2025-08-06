# LinearProbe Class Documentation

## Class Comment

LinearProbe is a specialized analysis tool that implements linear probing techniques for understanding what information is encoded in transformer model representations. Linear probes are simple linear classifiers trained to predict specific properties or features from model activations, providing insights into what information is linearly accessible at different layers and positions.

This class provides comprehensive capabilities for probe design, training, evaluation, and interpretation. It enables researchers to systematically investigate what linguistic, semantic, or task-specific information is encoded in model representations and how this information evolves across layers.

### Key Responsibilities

- **Probe Training**: Trains linear classifiers to predict target properties from model activations
- **Feature Extraction**: Extracts relevant features from model representations for probe training
- **Performance Evaluation**: Assesses probe accuracy and provides detailed performance metrics
- **Regularization Management**: Implements various regularization techniques to ensure probe validity
- **Interpretation Analysis**: Analyzes probe weights and predictions to understand representation content
- **Comparative Studies**: Enables comparison of probe performance across layers, models, or conditions

### Instance Variables

- `model` - The TransformerModel instance being probed
- `probeClassifiers` - Dictionary storing trained probe classifiers by target property
- `trainingData` - Dataset containing activation-label pairs for probe training
- `evaluationMetrics` - Collection of metrics for assessing probe performance
- `regularizationConfig` - Configuration specifying regularization parameters
- `featureExtractor` - Component for extracting relevant features from activations
- `interpretationTools` - Tools for analyzing probe weights and decision boundaries

### Probe Training and Evaluation Procedures

The LinearProbe implements sophisticated procedures for reliable probe training and evaluation:

#### Basic Probe Training
Trains linear classifiers to predict target properties from model activations.

```smalltalk
"Train a probe to predict part-of-speech tags"
probe := LinearProbe for: model.
posProbe := probe trainProbeFor: #partOfSpeech
    usingActivationsFrom: (layer: 8)
    withTrainingData: posTaggedSentences
    regularization: #l2
    regularizationStrength: 0.01.

"Evaluate probe performance"
accuracy := posProbe evaluateOn: testSentences.
Transcript show: 'POS tagging accuracy: ', accuracy asString; cr.
```

#### Multi-Layer Probe Analysis
Systematically trains probes across all model layers to understand information flow.

```smalltalk
"Analyze how syntactic information develops across layers"
probe := LinearProbe for: model.
syntaxResults := probe analyzeAcrossLayers: #syntacticRole
    fromLayer: 1 toLayer: 12
    usingDataset: syntaxDataset
    withCrossValidation: 5.

"Plot accuracy by layer"
syntaxResults keysAndValuesDo: [:layer :accuracy |
    Transcript show: 'Layer ', layer asString, ': ', accuracy asString; cr].
```

#### Probe Setup and Configuration
Provides flexible configuration options for different probing scenarios.

```smalltalk
"Configure probe with specific parameters"
probe := LinearProbe for: model.
probe configureTraining: 
    (optimizer: #adam
     learningRate: 0.001
     batchSize: 32
     epochs: 100
     earlyStopping: true
     patience: 10).

probe configureRegularization:
    (type: #elasticNet
     l1Strength: 0.001
     l2Strength: 0.01
     dropoutRate: 0.1).
```

### Regularization Options and Techniques

The LinearProbe implements various regularization techniques to ensure probe validity and prevent overfitting:

#### L1 and L2 Regularization
```smalltalk
"Train probe with L2 regularization"
l2Probe := probe trainProbeFor: #sentiment
    usingActivationsFrom: (layer: 10)
    withRegularization: #l2
    strength: 0.01.

"Train probe with L1 regularization for sparsity"
l1Probe := probe trainProbeFor: #sentiment
    usingActivationsFrom: (layer: 10)
    withRegularization: #l1
    strength: 0.005.
```

#### Elastic Net Regularization
```smalltalk
"Combine L1 and L2 regularization"
elasticProbe := probe trainProbeFor: #namedEntity
    usingActivationsFrom: (layer: 6)
    withRegularization: #elasticNet
    l1Strength: 0.001
    l2Strength: 0.01.
```

#### Dropout Regularization
```smalltalk
"Use dropout during probe training"
dropoutProbe := probe trainProbeFor: #semanticRole
    usingActivationsFrom: (layer: 9)
    withDropout: 0.2
    trainingEpochs: 50.
```

#### Control Task Regularization
```smalltalk
"Use control tasks to validate probe meaningfulness"
controlProbe := probe trainWithControlTask:
    mainTask: #syntacticDependency
    controlTask: #randomLabels
    activationLayer: 7
    significanceThreshold: 0.05.
```

### Performance Metrics and Evaluation

The LinearProbe provides comprehensive evaluation metrics for assessing probe performance:

#### Standard Classification Metrics
```smalltalk
"Evaluate probe with comprehensive metrics"
evaluation := probe evaluateProbe: trainedProbe
    onDataset: testDataset
    withMetrics: #(accuracy precision recall f1Score).

evaluation keysAndValuesDo: [:metric :value |
    Transcript show: metric, ': ', value asString; cr].
```

#### Cross-Validation Analysis
```smalltalk
"Perform k-fold cross-validation"
cvResults := probe crossValidateProbe: #partOfSpeech
    usingActivationsFrom: (layer: 8)
    folds: 5
    stratified: true.

Transcript show: 'Mean accuracy: ', cvResults meanAccuracy asString; cr.
Transcript show: 'Std deviation: ', cvResults stdDeviation asString; cr.
```

#### Statistical Significance Testing
```smalltalk
"Test statistical significance of probe performance"
significance := probe testSignificance: probeResults
    againstBaseline: #randomClassifier
    usingTest: #tTest
    alpha: 0.05.

Transcript show: 'Statistically significant: ', significance isSignificant asString; cr.
```

#### Learning Curve Analysis
```smalltalk
"Analyze how probe performance scales with training data"
learningCurve := probe analyzeLearningCurve: #sentiment
    trainingSizes: #(100 500 1000 5000 10000)
    activationLayer: 10
    repetitions: 3.

learningCurve do: [:point |
    Transcript show: point trainingSize asString, ' samples: ', point accuracy asString; cr].
```

### Probe Interpretation and Analysis

The LinearProbe includes tools for interpreting trained probes and understanding what they learn:

#### Weight Analysis
```smalltalk
"Analyze probe weights to understand important features"
weightAnalysis := probe analyzeWeights: trainedProbe
    featureNames: vocabularyTokens
    topFeatures: 20.

weightAnalysis positiveWeights do: [:feature |
    Transcript show: 'Positive: ', feature name, ' (', feature weight asString, ')'; cr].
```

#### Attention to Probe Predictions
```smalltalk
"Visualize which input tokens contribute most to probe predictions"
attentionMap := probe computeInputAttention: trainedProbe
    forInput: 'The quick brown fox jumps'
    targetClass: #noun.

attentionMap do: [:tokenAttention |
    Transcript show: tokenAttention token, ': ', tokenAttention attention asString; cr].
```

#### Decision Boundary Visualization
```smallttml
"Visualize probe decision boundaries in activation space"
boundaryViz := probe visualizeDecisionBoundary: trainedProbe
    inActivationSpace: (layer: 8)
    projectionMethod: #pca
    dimensions: 2.

boundaryViz display.
```

### Advanced Probing Techniques

The LinearProbe supports several advanced probing methodologies:

#### Causal Probing
```smalltalk
"Test whether probe predictions are causally related to model behavior"
causalProbe := probe performCausalProbing: #syntacticRole
    interventionMethod: #activationPatching
    behaviorMetric: #nextTokenPrediction
    causalityThreshold: 0.1.
```

#### Temporal Probing
```smalltalk
"Analyze how probe predictions change over sequence positions"
temporalAnalysis := probe analyzeTemporalDynamics: #sentiment
    acrossPositions: (1 to: 20)
    inLayer: 10
    averageOverExamples: 1000.
```

#### Hierarchical Probing
```smalltalk
"Train hierarchical probes for structured prediction tasks"
hierarchicalProbe := probe trainHierarchicalProbe: #syntacticParse
    withStructure: parseTreeStructure
    usingActivationsFrom: (layer: 8)
    hierarchyLevels: 3.
```

#### Multi-Task Probing
```smalltalk
"Train probes for multiple related tasks simultaneously"
multiTaskProbe := probe trainMultiTaskProbe: 
    tasks: #(partOfSpeech namedEntity syntacticRole)
    sharedLayers: 2
    taskSpecificLayers: 1
    activationLayer: 9.
```

### Computational Complexity and Performance

The LinearProbe is designed for efficient training and evaluation:

#### Training Complexity
- **Linear Probe Training**: O(n × d × e) where n is samples, d is dimension, e is epochs
- **Cross-Validation**: O(k × n × d × e) where k is number of folds
- **Multi-Layer Analysis**: O(l × n × d × e) where l is number of layers
- **Regularization Path**: O(r × n × d × e) where r is regularization values tested

#### Memory Requirements
- **Activation Storage**: ~4 bytes × num_samples × activation_dimension
- **Model Parameters**: ~4 bytes × activation_dimension × num_classes
- **Gradient Computation**: ~8 bytes × activation_dimension × num_classes
- **Cross-Validation**: Additional factor of k for k-fold CV

### Usage Patterns

#### Basic Probe Training and Evaluation
```smalltalk
"Train and evaluate a basic probe"
probe := LinearProbe for: model.

"Extract activations for training"
activations := probe extractActivations: trainingTexts fromLayer: 8.

"Train probe"
sentimentProbe := probe trainClassifier: activations 
    withLabels: sentimentLabels
    regularization: 0.01.

"Evaluate on test set"
testActivations := probe extractActivations: testTexts fromLayer: 8.
accuracy := sentimentProbe evaluateOn: testActivations withLabels: testLabels.
```

#### Systematic Layer Analysis
```smalltalk
"Analyze probe performance across all layers"
probe := LinearProbe for: model.
layerResults := Dictionary new.

1 to: model numLayers do: [:layerNum |
    layerProbe := probe trainProbeFor: #partOfSpeech
        usingActivationsFrom: (layer: layerNum)
        withCrossValidation: 5.
    layerResults at: layerNum put: layerProbe accuracy].

"Find best layer for POS information"
bestLayer := layerResults keyAtValue: layerResults values max.
```

#### Comparative Probe Analysis
```smalltalk
"Compare probe performance across different models"
models := #(gpt2Small gpt2Medium gpt2Large).
probeComparison := Dictionary new.

models do: [:modelName |
    model := TransformerModel fromHuggingFace: modelName.
    probe := LinearProbe for: model.
    result := probe trainProbeFor: #sentiment usingActivationsFrom: (layer: 8).
    probeComparison at: modelName put: result accuracy].
```

### Integration with Visualization

The LinearProbe integrates with the visualization system for interactive analysis:

```smalltalk
"Create interactive probe analysis visualization"
probe := LinearProbe for: model.
probeResults := probe trainProbeFor: #sentiment usingActivationsFrom: (layer: 10).

"Visualize probe performance across layers"
layerAnalysis := probe analyzeAcrossLayers: #sentiment.
visualization := probe visualizeLayerAnalysis: layerAnalysis.

"Open in interactive lens"
lens := InteractiveLens for: model.
lens showProbeAnalysis: visualization.
```

### Error Handling and Validation

The LinearProbe includes comprehensive error handling and validation:

- **Data Validation**: Checks for proper activation-label alignment and data quality
- **Convergence Monitoring**: Detects training convergence issues and provides diagnostics
- **Overfitting Detection**: Monitors validation performance to detect overfitting
- **Statistical Validation**: Ensures probe performance is statistically meaningful
- **Regularization Validation**: Validates that regularization parameters are appropriate

### Browser Integration and Optimization

When running in SqueakJS, the LinearProbe leverages browser capabilities:

- **WebGL Acceleration**: GPU-accelerated matrix operations for probe training
- **Web Workers**: Background probe training without blocking the UI
- **Progressive Training**: Real-time display of training progress and metrics
- **Memory Management**: Efficient handling of large activation datasets
- **Persistent Storage**: Caching of trained probes for reuse across sessions

This comprehensive linear probing capability makes LinearProbe essential for understanding what information is encoded in transformer representations and how this information can be linearly accessed for various prediction tasks.