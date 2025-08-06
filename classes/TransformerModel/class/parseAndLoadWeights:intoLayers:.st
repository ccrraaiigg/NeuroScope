parseAndLoadWeights: weightsData intoLayers: layers
	"Parses the downloaded weights data and loads it into the transformer layers.
	
	Parameters:
	- weightsData (ByteArray): Raw binary data of the weights file
	- layers (OrderedCollection): Collection of TransformerLayer instances"
	
	| weightDict |
	
	"Parse weights based on file format"
	weightDict := (weightsData first: 8) = 'SAFETENS' asByteArray
		ifTrue: [self parseSafetensors: weightsData]
		ifFalse: [self parsePytorchWeights: weightsData].
	
	"Load weights into embedding layer if present"
	self loadEmbeddingWeights: weightDict.
	
	"Load weights into each transformer layer"
	layers withIndexDo: [:layer :index |
		self loadLayerWeights: weightDict layer: layer index: index - 1
	].
	
	"Load final layer norm and output projection weights"
	self loadFinalWeights: weightDict