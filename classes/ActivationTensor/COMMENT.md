# ActivationTensor Class Documentation

## Purpose

ActivationTensor represents the fundamental data structure for storing and manipulating neural network activations within the NeuroScope framework. It encapsulates tensor data along with metadata about shape, device placement, gradient requirements, and provides integration points for the hook system.

## Responsibilities

- **Tensor Data Management**: Store and provide access to multi-dimensional activation data
- **Shape and Metadata Tracking**: Maintain tensor dimensions, device placement, and gradient computation flags
- **Hook Integration**: Support attachment of hooks for activation monitoring and intervention
- **Memory Management**: Handle efficient storage and cleanup of tensor data in browser environments
- **Mathematical Operations**: Provide tensor arithmetic and manipulation capabilities
- **Browser Optimization**: Leverage WebGL and typed arrays for performance in web environments

## Key Concepts

ActivationTensor bridges the gap between raw numerical data and the object-oriented analysis capabilities of NeuroScope. Each tensor represents a snapshot of neural network state at a specific point in computation, enriched with metadata that enables sophisticated interpretability analyses.

The class supports both CPU-based operations for compatibility and WebGL-accelerated operations for performance, automatically selecting the appropriate backend based on browser capabilities and tensor size.

## Instance Variables

- **data**: The raw tensor data, stored as either a nested array structure or a typed array for efficiency. Contains the actual activation values from neural network computation.

- **shape**: An array describing the tensor dimensions (e.g., #(batch_size sequence_length hidden_size)). Essential for tensor operations and reshaping.

- **device**: Specifies where the tensor data resides ('cpu', 'webgl', 'javascript'). Determines which computational backend to use for operations.

- **requiresGrad**: Boolean flag indicating whether this tensor should track gradients for backpropagation. Important for probe training and gradient-based analysis.

- **hooks**: Collection of Hook objects attached to this tensor. Enables monitoring, caching, and intervention capabilities during forward passes.

## Usage Patterns

### Basic Tensor Creation
```smalltalk
"Create tensor from raw data"
tensor := ActivationTensor 
    data: #((1.0 2.0 3.0) (4.0 5.0 6.0))
    shape: #(2 3).

"Create tensor with gradient tracking"
gradTensor := ActivationTensor 
    data: activationData
    shape: #(1 512 768)
    requiresGrad: true.
```

### Hook Integration
```smallttml
"Attach monitoring hook"
monitorHook := ActivationHook 
    name: 'layer5_monitor'
    action: [:tensor | Transcript show: 'Activation: ', tensor mean asString].

tensor addHook: monitorHook.

"Attach intervention hook"
interventionHook := InterventionHook
    name: 'zero_negative'
    action: [:tensor | tensor data collect: [:val | val max: 0]].

tensor addHook: interventionHook.
```

### Mathematical Operations
```smalltalk
"Basic arithmetic"
result := tensor1 + tensor2.
scaled := tensor * 2.0.
normalized := tensor / tensor norm.

"Advanced operations"
dotProduct := tensor1 dot: tensor2.
reshaped := tensor reshape: #(batch_size -1). "Auto-infer last dimension"
sliced := tensor slice: (1 to: 10) axis: 1.
```

### Browser-Specific Operations
```smalltalk
"WebGL acceleration"
webglTensor := tensor moveToDevice: 'webgl'.
fastResult := webglTensor multiply: otherWebglTensor.

"Efficient data transfer"
jsArray := tensor asTypedArray. "For JavaScript interop"
fromJS := ActivationTensor fromTypedArray: jsArray shape: #(100 768).
```

## Integration Points

### With TransformerModel
ActivationTensor instances are created during forward passes and cached for analysis. The model manages tensor lifecycle and device placement.

### With Hook System
Tensors serve as the primary data objects that hooks operate on. Hook attachment enables real-time monitoring and intervention during computation.

### With Analysis Tools
Analysis classes (AttentionAnalyzer, NeuronAnalyzer) consume ActivationTensor instances to perform interpretability computations.

### With Visualization Components
Visualization classes render tensor data through Canvas and WebGL interfaces, with tensors providing optimized data access methods.

### With Browser APIs
Direct integration with JavaScript typed arrays, WebGL buffers, and Canvas ImageData for efficient browser-based computation and rendering.

## Examples

### Complete Forward Pass Integration
```smalltalk
"Extract and analyze layer activations"
model := TransformerModel fromHuggingFace: 'gpt2-small'.
tokens := model tokenizer encode: 'The cat sat on the mat'.

"Run with activation caching"
activations := model runWithCaching: tokens layers: #(0 6 11).

"Access specific layer tensor"
layer6Residual := activations at: 6 at: #residual.

"Analyze tensor properties"
Transcript show: 'Shape: ', layer6Residual shape asString.
Transcript show: 'Mean activation: ', layer6Residual mean asString.
Transcript show: 'Device: ', layer6Residual device.

"Apply analysis"
topNeurons := layer6Residual topK: 10 axis: -1.
attentionWeights := layer6Residual softmax: -1.
```

### Hook-Based Monitoring
```smalltalk
"Set up comprehensive monitoring"
tensor := ActivationTensor data: someData shape: #(1 512 768).

"Statistics hook"
statsHook := ActivationHook 
    name: 'statistics'
    action: [:t | 
        Dictionary new
            at: #mean put: t mean;
            at: #std put: t std;
            at: #max put: t max;
            yourself].

"Caching hook for later analysis"
cacheHook := CachingHook 
    name: 'layer_cache'
    maxSize: 100.

"Intervention hook for ablation"
ablationHook := InterventionHook
    name: 'zero_ablation'
    condition: [:t | ablationActive]
    action: [:t | t * 0].

tensor 
    addHook: statsHook;
    addHook: cacheHook;
    addHook: ablationHook.

"Process tensor through model"
result := model processActivation: tensor.

"Access hook results"
statistics := statsHook lastResult.
cachedValues := cacheHook cachedActivations.
```

### WebGL Acceleration Example
```smalltalk
"Large tensor operations with GPU acceleration"
largeTensor := ActivationTensor 
    data: (Array new: 1000000 withAll: 0.5)
    shape: #(1000 1000)
    device: 'webgl'.

"Fast matrix operations"
weights := ActivationTensor 
    data: randomWeights
    shape: #(1000 768)
    device: 'webgl'.

"GPU-accelerated multiplication"
output := largeTensor matmul: weights. "Uses WebGL compute shaders"

"Transfer back to CPU for analysis"
cpuResult := output moveToDevice: 'cpu'.
analysis := AttentionAnalyzer analyze: cpuResult.
```

### Memory Management
```smalltalk
"Efficient memory usage patterns"
tensor := ActivationTensor data: largeData shape: #(100 768).

"Automatic cleanup when out of scope"
tensor onFinalize: [tensor cleanup].

"Manual memory management"
tensor 
    clearCache;
    releaseGPUMemory;
    compactStorage.

"Lazy evaluation for large datasets"
lazyTensor := ActivationTensor 
    lazyData: [:shape | self computeExpensiveActivation: shape]
    shape: #(1000 768).

"Data computed only when accessed"
firstValue := lazyTensor at: 1 at: 1. "Triggers computation"
```

This comprehensive documentation provides developers with a complete understanding of ActivationTensor's role in the NeuroScope framework, from basic usage to advanced browser-specific optimizations.
## 
Method Documentation

### Class Methods

#### data: dataArray shape: shapeArray
**Purpose**: Creates a new ActivationTensor with the specified data and shape.

**Parameters**:
- `dataArray` (Array or TypedArray): The tensor data as nested arrays or flat typed array
- `shapeArray` (Array of Integer): Dimensions of the tensor (e.g., #(batch_size sequence_length hidden_size))

**Return Value**: A new ActivationTensor instance

**Usage Examples**:
```smalltalk
"Create 2D tensor from nested arrays"
tensor := ActivationTensor 
    data: #((1.0 2.0 3.0) (4.0 5.0 6.0))
    shape: #(2 3).

"Create 3D tensor from flat array"
flatData := (1 to: 24) collect: [:i | i asFloat].
tensor3D := ActivationTensor 
    data: flatData
    shape: #(2 3 4).

"Create tensor with typed array for efficiency"
typedData := Float32Array new: 1000.
largeTensor := ActivationTensor 
    data: typedData
    shape: #(10 100).
```

**Error Conditions**:
- Raises `ShapeMismatchError` if data size doesn't match shape dimensions
- Raises `InvalidDataError` if data contains non-numeric values

#### fromTypedArray: typedArray shape: shapeArray
**Purpose**: Creates an ActivationTensor from a JavaScript typed array for efficient browser interop.

**Parameters**:
- `typedArray` (TypedArray): JavaScript typed array (Float32Array, Float64Array, etc.)
- `shapeArray` (Array of Integer): Tensor dimensions

**Return Value**: A new ActivationTensor optimized for browser operations

**Usage Examples**:
```smalltalk
"Create from JavaScript Float32Array"
jsArray := Float32Array new: 768.
tensor := ActivationTensor fromTypedArray: jsArray shape: #(1 768).

"Efficient data transfer from WebGL"
webglBuffer := webglContext readPixels: 0 at: 0 width: 256 height: 256.
imageTensor := ActivationTensor 
    fromTypedArray: webglBuffer 
    shape: #(256 256 4).
```

**Performance Notes**: This method provides zero-copy construction when possible, making it ideal for JavaScript interop.

#### zeros: shapeArray
**Purpose**: Creates a tensor filled with zeros of the specified shape.

**Parameters**:
- `shapeArray` (Array of Integer): Desired tensor dimensions

**Return Value**: A new ActivationTensor filled with zeros

**Usage Examples**:
```smalltalk
"Create zero tensor for initialization"
zeroTensor := ActivationTensor zeros: #(100 768).

"Create batch of zero vectors"
zeroBatch := ActivationTensor zeros: #(32 512 768).
```

#### ones: shapeArray
**Purpose**: Creates a tensor filled with ones of the specified shape.

**Parameters**:
- `shapeArray` (Array of Integer): Desired tensor dimensions

**Return Value**: A new ActivationTensor filled with ones

#### random: shapeArray
**Purpose**: Creates a tensor filled with random values from a normal distribution.

**Parameters**:
- `shapeArray` (Array of Integer): Desired tensor dimensions

**Return Value**: A new ActivationTensor with random initialization

**Usage Examples**:
```smalltalk
"Random tensor for testing"
randomTensor := ActivationTensor random: #(10 768).

"Random initialization with specific distribution"
gaussianTensor := ActivationTensor 
    random: #(100 100)
    mean: 0.0
    std: 0.02.
```

### Instance Methods

#### at: indices
**Purpose**: Accesses tensor elements at the specified multi-dimensional indices.

**Parameters**:
- `indices` (Array of Integer): Indices for each dimension

**Return Value**: The value at the specified position (Number or ActivationTensor for slices)

**Usage Examples**:
```smalltalk
"Access single element"
value := tensor at: #(0 5 100).

"Access with variable indices"
batchIndex := 0.
seqIndex := 10.
hiddenIndex := 256.
activation := tensor at: {batchIndex. seqIndex. hiddenIndex}.
```

**Error Conditions**:
- Raises `IndexOutOfBoundsError` if any index exceeds tensor dimensions

#### at: indices put: value
**Purpose**: Sets the tensor element at the specified indices to the given value.

**Parameters**:
- `indices` (Array of Integer): Target position indices
- `value` (Number): Value to store

**Return Value**: The stored value

**Side Effects**: Modifies the tensor data in place

**Usage Examples**:
```smalltalk
"Set single element"
tensor at: #(0 0 0) put: 1.5.

"Batch update with iteration"
(0 to: tensor shape first - 1) do: [:i |
    tensor at: {i. 0. 0} put: i asFloat
].
```

#### slice: range axis: axisIndex
**Purpose**: Extracts a slice of the tensor along the specified axis.

**Parameters**:
- `range` (Interval or Array): Indices to extract along the axis
- `axisIndex` (Integer): Which axis to slice (0-based)

**Return Value**: A new ActivationTensor containing the sliced data

**Usage Examples**:
```smalltalk
"Extract sequence slice"
firstTenTokens := tensor slice: (0 to: 9) axis: 1.

"Extract specific batch elements"
selectedBatches := tensor slice: #(0 2 4 6) axis: 0.

"Extract hidden dimension subset"
firstHalf := tensor slice: (0 to: 383) axis: 2.
```

**Performance Notes**: Creates a new tensor; use views for memory efficiency when possible.

#### reshape: newShape
**Purpose**: Returns a new tensor with the same data but different shape dimensions.

**Parameters**:
- `newShape` (Array of Integer): New tensor dimensions (-1 can be used to auto-infer one dimension)

**Return Value**: A new ActivationTensor with the specified shape

**Usage Examples**:
```smalltalk
"Flatten tensor"
flattened := tensor reshape: #(-1).

"Reshape for matrix operations"
matrix := tensor reshape: #(100 768).

"Auto-infer batch dimension"
reshaped := tensor reshape: #(-1 768). "Infers first dimension"
```

**Error Conditions**:
- Raises `ShapeError` if new shape doesn't match total element count

#### transpose: axes
**Purpose**: Reorders tensor dimensions according to the specified axis permutation.

**Parameters**:
- `axes` (Array of Integer): New order of axes (optional, defaults to reverse order)

**Return Value**: A new ActivationTensor with transposed dimensions

**Usage Examples**:
```smalltalk
"Simple matrix transpose"
transposed := matrix transpose.

"Custom axis reordering for attention weights"
"From [batch, heads, seq_len, seq_len] to [batch, seq_len, heads, seq_len]"
reordered := attentionTensor transpose: #(0 2 1 3).
```

#### + otherTensor
**Purpose**: Element-wise addition with another tensor or scalar.

**Parameters**:
- `otherTensor` (ActivationTensor or Number): Tensor or scalar to add

**Return Value**: A new ActivationTensor containing the sum

**Usage Examples**:
```smalltalk
"Tensor addition"
sum := tensor1 + tensor2.

"Scalar addition (broadcasting)"
shifted := tensor + 1.0.

"Bias addition"
withBias := activations + biasVector.
```

**Error Conditions**:
- Raises `ShapeError` if tensor shapes are incompatible for broadcasting

#### - otherTensor
**Purpose**: Element-wise subtraction with another tensor or scalar.

**Parameters**:
- `otherTensor` (ActivationTensor or Number): Tensor or scalar to subtract

**Return Value**: A new ActivationTensor containing the difference

#### * otherTensor
**Purpose**: Element-wise multiplication with another tensor or scalar.

**Parameters**:
- `otherTensor` (ActivationTensor or Number): Tensor or scalar to multiply

**Return Value**: A new ActivationTensor containing the product

**Usage Examples**:
```smalltalk
"Element-wise multiplication"
product := tensor1 * tensor2.

"Scaling"
scaled := tensor * 0.5.

"Attention masking"
maskedAttention := attentionWeights * attentionMask.
```

#### / otherTensor
**Purpose**: Element-wise division with another tensor or scalar.

**Parameters**:
- `otherTensor` (ActivationTensor or Number): Tensor or scalar to divide by

**Return Value**: A new ActivationTensor containing the quotient

#### matmul: otherTensor
**Purpose**: Performs matrix multiplication between two tensors.

**Parameters**:
- `otherTensor` (ActivationTensor): Tensor to multiply with (compatible dimensions required)

**Return Value**: A new ActivationTensor containing the matrix product

**Usage Examples**:
```smalltalk
"Standard matrix multiplication"
output := input matmul: weights.

"Batch matrix multiplication"
batchOutput := batchInput matmul: weightMatrix.

"Attention computation"
attentionScores := queries matmul: keys transpose.
```

**Performance Notes**: Automatically uses WebGL acceleration when available and beneficial.

#### dot: otherTensor
**Purpose**: Computes the dot product between two tensors.

**Parameters**:
- `otherTensor` (ActivationTensor): Tensor to compute dot product with

**Return Value**: A scalar value (Number) representing the dot product

**Usage Examples**:
```smalltalk
"Vector similarity"
similarity := vector1 dot: vector2.

"Projection computation"
projection := (vector dot: direction) * direction.
```

#### norm
**Purpose**: Computes the L2 (Euclidean) norm of the tensor.

**Return Value**: A scalar value (Number) representing the tensor's norm

**Usage Examples**:
```smalltalk
"Vector magnitude"
magnitude := vector norm.

"Normalization"
normalized := vector / vector norm.

"Gradient clipping"
gradNorm := gradients norm.
gradNorm > maxNorm ifTrue: [
    gradients := gradients * (maxNorm / gradNorm)
].
```

#### mean
**Purpose**: Computes the mean value of all tensor elements.

**Return Value**: A scalar value (Number) representing the mean

**Usage Examples**:
```smalltalk
"Overall activation level"
avgActivation := tensor mean.

"Layer statistics"
layerStats := Dictionary new
    at: #mean put: tensor mean;
    at: #std put: tensor std;
    yourself.
```

#### mean: axis
**Purpose**: Computes the mean along the specified axis.

**Parameters**:
- `axis` (Integer): Axis along which to compute the mean

**Return Value**: A new ActivationTensor with reduced dimensions

**Usage Examples**:
```smalltalk
"Average across sequence dimension"
avgOverTime := tensor mean: 1.

"Average across batch dimension"
batchMean := tensor mean: 0.

"Token-wise averages"
tokenAverages := tensor mean: -1. "Last dimension"
```

#### std
**Purpose**: Computes the standard deviation of all tensor elements.

**Return Value**: A scalar value (Number) representing the standard deviation

#### max
**Purpose**: Finds the maximum value in the tensor.

**Return Value**: A scalar value (Number) representing the maximum

#### min
**Purpose**: Finds the minimum value in the tensor.

**Return Value**: A scalar value (Number) representing the minimum

#### topK: k axis: axis
**Purpose**: Finds the k largest values along the specified axis.

**Parameters**:
- `k` (Integer): Number of top values to return
- `axis` (Integer): Axis along which to find top values

**Return Value**: A Dictionary containing 'values' and 'indices' arrays

**Usage Examples**:
```smalltalk
"Top predicted tokens"
topTokens := logits topK: 10 axis: -1.
topValues := topTokens at: #values.
topIndices := topTokens at: #indices.

"Most active neurons"
topNeurons := activations topK: 20 axis: 1.
```

#### softmax: axis
**Purpose**: Applies the softmax function along the specified axis.

**Parameters**:
- `axis` (Integer): Axis along which to apply softmax

**Return Value**: A new ActivationTensor with softmax-normalized values

**Usage Examples**:
```smalltalk
"Convert logits to probabilities"
probabilities := logits softmax: -1.

"Attention weight normalization"
attentionWeights := attentionScores softmax: -1.
```

#### addHook: hook
**Purpose**: Attaches a hook to this tensor for monitoring or intervention.

**Parameters**:
- `hook` (Hook): Hook instance to attach

**Return Value**: Self (for method chaining)

**Side Effects**: Hook will be executed when tensor is accessed or modified

**Usage Examples**:
```smalltalk
"Add monitoring hook"
tensor addHook: (ActivationHook 
    name: 'monitor'
    action: [:t | Transcript show: 'Accessed: ', t shape asString]).

"Add caching hook"
tensor addHook: (CachingHook name: 'cache' maxSize: 100).

"Method chaining"
tensor 
    addHook: monitorHook;
    addHook: cacheHook;
    addHook: interventionHook.
```

#### removeHook: hook
**Purpose**: Removes a previously attached hook from this tensor.

**Parameters**:
- `hook` (Hook): Hook instance to remove

**Return Value**: Boolean indicating whether the hook was found and removed

#### clearHooks
**Purpose**: Removes all hooks attached to this tensor.

**Return Value**: Self (for method chaining)

#### moveToDevice: deviceName
**Purpose**: Transfers tensor data to the specified computational device.

**Parameters**:
- `deviceName` (String): Target device ('cpu', 'webgl', 'javascript')

**Return Value**: A new ActivationTensor on the specified device

**Usage Examples**:
```smalltalk
"Move to GPU for acceleration"
gpuTensor := tensor moveToDevice: 'webgl'.

"Move back to CPU for analysis"
cpuTensor := gpuTensor moveToDevice: 'cpu'.

"Optimize for JavaScript interop"
jsTensor := tensor moveToDevice: 'javascript'.
```

**Performance Notes**: Device transfers can be expensive; minimize unnecessary moves.

#### asTypedArray
**Purpose**: Converts tensor data to a JavaScript typed array for efficient interop.

**Return Value**: A TypedArray (Float32Array, Float64Array, etc.) containing the tensor data

**Usage Examples**:
```smalltalk
"Export for JavaScript processing"
jsArray := tensor asTypedArray.

"WebGL buffer creation"
buffer := webglContext createBuffer.
webglContext bufferData: jsArray.

"Canvas ImageData creation"
imageData := canvas createImageData: width height: height.
imageData data: tensor asTypedArray.
```

#### copy
**Purpose**: Creates a deep copy of the tensor with independent data.

**Return Value**: A new ActivationTensor with copied data

**Usage Examples**:
```smalltalk
"Safe copy for modification"
backup := tensor copy.

"Independent processing"
modified := tensor copy.
modified *= 2.0.
```

#### requiresGrad: boolean
**Purpose**: Sets whether this tensor should track gradients for backpropagation.

**Parameters**:
- `boolean` (Boolean): Whether to track gradients

**Return Value**: Self (for method chaining)

**Side Effects**: Affects gradient computation in subsequent operations

**Usage Examples**:
```smalltalk
"Enable gradient tracking for probe training"
tensor requiresGrad: true.

"Disable gradients for inference"
tensor requiresGrad: false.
```

#### cleanup
**Purpose**: Releases tensor resources and clears cached data for memory management.

**Return Value**: Self

**Side Effects**: Frees GPU memory and clears caches

**Usage Examples**:
```smalltalk
"Manual cleanup for large tensors"
largeTensor cleanup.

"Cleanup after analysis"
analysisResults do: [:tensor | tensor cleanup].
```