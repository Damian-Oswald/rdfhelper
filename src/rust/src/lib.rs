use extendr_api::prelude::*;
use oxigraph::store::Store;
use oxigraph::model::*;
use oxigraph::sparql::QueryResults;

// 1. Tell extendr to wrap this struct in an R External Pointer
#[extendr]
pub struct OxStore {
    inner: Store,
}

// 2. Export the methods to R
#[extendr]
impl OxStore {
    
    fn new() -> Self {
        Self {
            inner: Store::new().expect("Failed to initialize Oxigraph store"),
        }
    }

    fn insert(&self, subject: &str, predicate: &str, object: &str) {
        let s = NamedNode::new(subject).expect("Invalid subject URI");
        let p = NamedNode::new(predicate).expect("Invalid predicate URI");
        let o = NamedNode::new(object).expect("Invalid object URI");
        
        let quad = Quad::new(s, p, o, GraphName::DefaultGraph);
        
        self.inner.insert(&quad).expect("Failed to insert quad");
    }

    fn ask(&self, query: &str) -> bool {
        let results = self.inner.query(query).expect("Query failed");
        
        if let QueryResults::Boolean(b) = results {
            b
        } else {
            false
        }
    }
}

// 3. Register the struct
extendr_module! {
    mod rdfhelper;
    impl OxStore;
}