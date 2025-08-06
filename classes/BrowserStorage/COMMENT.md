# BrowserStorage Class Documentation

## Class Comment

BrowserStorage provides a unified interface for data persistence and caching in web browsers, abstracting over multiple browser storage mechanisms including localStorage, sessionStorage, IndexedDB, and Cache API. This class enables efficient storage and retrieval of neural network models, activation data, analysis results, and user preferences within the browser environment, supporting both temporary caching and persistent storage needs.

### Purpose and Responsibilities

BrowserStorage manages the complex landscape of browser storage options, providing automatic storage selection based on data size, persistence requirements, and browser capabilities. It handles data serialization, compression, and cleanup while maintaining compatibility across different browser implementations and storage quotas.

Key responsibilities include:
- **Storage Strategy Selection**: Automatically choose optimal storage mechanism based on data characteristics
- **Data Serialization**: Convert Smalltalk objects to browser-compatible formats
- **Quota Management**: Monitor and manage browser storage quotas across different APIs
- **Cache Invalidation**: Implement intelligent cache expiration and cleanup policies
- **Cross-Session Persistence**: Maintain data across browser sessions when required

### Instance Variables

- **storageBackends**: Dictionary of available storage mechanisms (localStorage, IndexedDB, etc.)
- **quotaManager**: Monitors storage usage and available space across all backends
- **serializationStrategy**: Handles conversion between Smalltalk objects and storage formats
- **cachePolicy**: Defines expiration, eviction, and cleanup rules
- **compressionEngine**: Manages data compression for large objects
- **encryptionProvider**: Optional encryption for sensitive data storage

### Storage Backend Architecture

#### Automatic Backend Selection
```smalltalk
"Storage automatically selects optimal backend"
storage := BrowserStorage new.

"Small configuration data → localStorage"
storage store: userPreferences key: 'user-config'.

"Large model weights → IndexedDB"
storage store: modelWeights key: 'gpt2-small-weights'.

"Temporary analysis results → sessionStorage"
storage storeTemporary: analysisResults key: 'attention-patterns'.

"Check which backend was selected"
storage backendUsedFor: 'gpt2-small-weights'.  "Returns #indexedDB"
```

#### Manual Backend Selection
```smalltalk
"Force specific storage backend when needed"
storage := BrowserStorage new.

"Store in specific backend"
storage 
    store: largeDataset 
    key: 'activation-cache' 
    backend: #indexedDB.

"Store with specific options"
storage 
    store: sensitiveData 
    key: 'user-tokens'
    backend: #localStorage
    options: (Dictionary new at: 'encrypt' put: true; yourself).
```

### IndexedDB Integration

#### Database Schema Management
```smalltalk
"Initialize IndexedDB schema for NeuroScope data"
BrowserStorage class>>initializeIndexedDB
    | schema |
    schema := IndexedDBSchema new
        database: 'NeuroScope'
        version: 1.
    
    "Object stores for different data types"
    schema 
        addObjectStore: 'models' 
        keyPath: 'name'
        autoIncrement: false.
        
    schema 
        addObjectStore: 'activations'
        keyPath: 'id'
        autoIncrement: true.
        
    schema 
        addObjectStore: 'analysis-results'
        keyPath: 'timestamp'
        autoIncrement: false.
    
    "Create indexes for efficient querying"
    schema 
        addIndex: 'models' 
        name: 'by-size' 
        keyPath: 'size'.
        
    ^schema
```

#### Large Object Storage
```smalltalk
"Store large neural network models"
model := TransformerModel fromHuggingFace: 'gpt2-small'.
modelData := Dictionary new
    at: 'name' put: 'gpt2-small';
    at: 'weights' put: model weights;
    at: 'config' put: model config;
    at: 'size' put: model estimatedSize;
    at: 'timestamp' put: DateAndTime now;
    yourself.

storage storeInIndexedDB: modelData objectStore: 'models'.

"Retrieve with query"
retrievedModel := storage 
    queryIndexedDB: 'models' 
    where: [:record | record name = 'gpt2-small'].
```

#### Batch Operations
```smalltalk
"Efficiently store multiple activation tensors"
activations := Array new: 1000.
1 to: 1000 do: [:i |
    activations at: i put: (Dictionary new
        at: 'layer' put: i;
        at: 'data' put: (ActivationTensor randomShape: #(768));
        at: 'timestamp' put: DateAndTime now;
        yourself).
].

"Batch insert for better performance"
storage batchStoreInIndexedDB: activations objectStore: 'activations'.
```

### localStorage Integration

#### Configuration Storage
```smallttml
"Store user preferences and settings"
preferences := Dictionary new
    at: 'theme' put: 'dark';
    at: 'defaultModel' put: 'gpt2-small';
    at: 'visualizationSettings' put: (Dictionary new
        at: 'showAttentionHeads' put: true;
        at: 'colorScheme' put: 'viridis';
        yourself);
    yourself.

storage storeInLocalStorage: preferences key: 'neuroscope-preferences'.

"Retrieve and apply settings"
savedPreferences := storage getFromLocalStorage: 'neuroscope-preferences'.
self applyUserPreferences: savedPreferences.
```

#### Session Management
```smalltalk
"Store session-specific data"
session := Dictionary new
    at: 'sessionId' put: UUID new asString;
    at: 'startTime' put: DateAndTime now;
    at: 'currentModel' put: model name;
    at: 'analysisHistory' put: OrderedCollection new;
    yourself.

storage storeInSessionStorage: session key: 'current-session'.
```

### Data Serialization and Compression

#### Smalltalk Object Serialization
```smalltalk
"Custom serialization for complex objects"
BrowserStorage>>serializeObject: anObject
    | serializer |
    serializer := self serializationStrategy.
    
    "Handle different object types"
    anObject class = ActivationTensor ifTrue: [
        ^serializer serializeTensor: anObject.
    ].
    
    anObject class = TransformerModel ifTrue: [
        ^serializer serializeModel: anObject.
    ].
    
    "Default JSON serialization"
    ^serializer toJSON: anObject
```

#### Compression for Large Data
```smalltalk
"Compress large datasets before storage"
largeActivationData := model getAllActivations: inputTokens.

"Compress using browser-native compression"
compressedData := storage compress: largeActivationData algorithm: #gzip.

"Store compressed data"
storage 
    store: compressedData 
    key: 'large-activation-set'
    metadata: (Dictionary new 
        at: 'compressed' put: true;
        at: 'originalSize' put: largeActivationData size;
        at: 'algorithm' put: #gzip;
        yourself).

"Automatic decompression on retrieval"
retrievedData := storage get: 'large-activation-set'.  "Automatically decompressed"
```

#### Binary Data Handling
```smalltalk
"Efficient storage of tensor data as binary"
tensor := ActivationTensor withData: (FloatArray new: 10000 withAll: 1.0).

"Convert to binary format"
binaryData := storage tensorToBinary: tensor.

"Store as Blob in IndexedDB"
storage 
    storeBlob: binaryData 
    key: 'tensor-binary'
    mimeType: 'application/octet-stream'.
```

### Storage Limitations and Quota Management

#### Quota Monitoring
```smalltalk
"Monitor storage usage across all backends"
BrowserStorage>>checkStorageQuotas
    | quotas |
    quotas := Dictionary new.
    
    "Check localStorage quota (typically 5-10MB)"
    quotas at: 'localStorage' put: self getLocalStorageQuota.
    
    "Check IndexedDB quota (typically 50MB-1GB+)"
    quotas at: 'indexedDB' put: self getIndexedDBQuota.
    
    "Check Cache API quota"
    quotas at: 'cacheAPI' put: self getCacheAPIQuota.
    
    ^quotas
```

#### Automatic Cleanup
```smalltalk
"Implement LRU cache eviction"
BrowserStorage>>enforceStorageQuota
    | usage quota |
    usage := self getTotalStorageUsage.
    quota := self getAvailableQuota.
    
    usage > (quota * 0.9) ifTrue: [
        "Approaching quota limit, start cleanup"
        self evictLeastRecentlyUsed: (usage - (quota * 0.7)).
    ].
```

#### Storage Estimation
```smalltalk
"Estimate storage requirements before storing"
BrowserStorage>>estimateStorageSize: object
    | estimatedSize |
    estimatedSize := object class = ActivationTensor 
        ifTrue: [object data size * 4]  "4 bytes per float"
        ifFalse: [self estimateJSONSize: object].
    
    "Add overhead for metadata and indexing"
    ^estimatedSize * 1.2
```

### Cleanup Procedures and Cache Management

#### Automatic Expiration
```smalltalk
"Set expiration policies for different data types"
storage := BrowserStorage new.

"Model weights persist across sessions"
storage 
    store: modelWeights 
    key: 'model-weights'
    expiration: #never.

"Analysis results expire after 24 hours"
storage 
    store: analysisResults 
    key: 'attention-analysis'
    expiration: (Duration days: 1).

"Temporary data expires when session ends"
storage 
    storeTemporary: tempData 
    key: 'temp-calculations'.
```

#### Manual Cleanup
```smalltalk
"Clean up expired data"
storage cleanupExpiredData.

"Clear specific data types"
storage clearModelCache.
storage clearAnalysisResults.

"Complete storage reset"
storage clearAllData.
```

#### Selective Cleanup
```smalltalk
"Clean up based on usage patterns"
storage 
    cleanupWhere: [:key :metadata | 
        metadata lastAccessed < (DateAndTime now - (Duration days: 7))].

"Clean up large unused items first"
storage 
    cleanupLargestItems: 10
    olderThan: (Duration days: 1).
```

### Error Handling and Recovery

#### Storage Failure Handling
```smalltalk
"Graceful degradation when storage fails"
BrowserStorage>>store: object key: key
    [
        "Try primary storage backend"
        ^self storeInPrimaryBackend: object key: key.
    ] on: StorageQuotaExceededError do: [:error |
        "Clean up and retry"
        self performEmergencyCleanup.
        ^self storeInFallbackBackend: object key: key.
    ] on: StorageNotAvailableError do: [:error |
        "Fall back to memory-only storage"
        ^self storeInMemory: object key: key.
    ].
```

#### Data Corruption Recovery
```smalltalk
"Detect and recover from corrupted data"
BrowserStorage>>get: key
    | data |
    data := self getRawData: key.
    
    (self validateData: data) ifFalse: [
        "Data corrupted, try backup"
        data := self getBackupData: key.
        (self validateData: data) ifFalse: [
            "No valid backup, return nil"
            ^nil.
        ].
    ].
    
    ^self deserializeData: data
```

### Integration with NeuroScope Components

#### Model Caching
```smalltalk
"Cache loaded models for quick access"
TransformerModel class>>fromHuggingFace: modelName
    | cachedModel |
    
    "Check cache first"
    cachedModel := BrowserStorage default get: 'model-' + modelName.
    cachedModel ifNotNil: [^cachedModel].
    
    "Load from network and cache"
    model := self loadFromNetwork: modelName.
    BrowserStorage default 
        store: model 
        key: 'model-' + modelName
        expiration: (Duration days: 7).
    
    ^model
```

#### Analysis Result Persistence
```smalltalk
"Save analysis results for later review"
AttentionAnalyzer>>analyzeAndSave: tokens model: model
    | results |
    results := self analyze: tokens model: model.
    
    "Store results with metadata"
    BrowserStorage default store: (Dictionary new
        at: 'results' put: results;
        at: 'model' put: model name;
        at: 'tokens' put: tokens;
        at: 'timestamp' put: DateAndTime now;
        at: 'analysisType' put: 'attention';
        yourself)
    key: 'analysis-' + UUID new asString.
    
    ^results
```

### Performance Optimization

#### Lazy Loading
```smalltalk
"Load data only when needed"
BrowserStorage>>lazyGet: key
    ^LazyValue new
        computation: [self get: key];
        yourself
```

#### Background Preloading
```smalltalk
"Preload frequently used data"
BrowserStorage>>preloadCommonData
    | commonKeys |
    commonKeys := #('user-preferences' 'default-model' 'recent-analyses').
    
    commonKeys do: [:key |
        self preloadInBackground: key.
    ].
```

#### Batch Operations
```smalltalk
"Batch multiple storage operations"
BrowserStorage>>batchStore: keyValuePairs
    | transaction |
    transaction := self beginTransaction.
    
    keyValuePairs do: [:pair |
        transaction store: pair value key: pair key.
    ].
    
    transaction commit.
```

### Best Practices and Usage Guidelines

1. **Choose Appropriate Backend**: Use localStorage for small config, IndexedDB for large data
2. **Monitor Quotas**: Regularly check storage usage and implement cleanup policies
3. **Handle Failures Gracefully**: Always provide fallback strategies for storage failures
4. **Compress Large Data**: Use compression for datasets larger than 1MB
5. **Set Appropriate Expiration**: Don't store temporary data permanently
6. **Validate Data Integrity**: Check for corruption when retrieving critical data
7. **Use Transactions**: Batch related operations for consistency and performance

### Common Usage Patterns

#### Initialization Pattern
```smalltalk
"Standard BrowserStorage setup"
storage := BrowserStorage new
    initializeBackends;
    setDefaultExpirationPolicy: (Duration days: 30);
    enableCompression: true;
    yourself.
```

#### Cleanup Pattern
```smalltalk
"Regular maintenance"
BrowserStorage>>performMaintenance
    self cleanupExpiredData.
    self defragmentIndexedDB.
    self updateQuotaEstimates.
    self logStorageStatistics.
```

This comprehensive documentation provides developers with complete guidance for implementing efficient, reliable data persistence and caching strategies within the browser-based NeuroScope framework.