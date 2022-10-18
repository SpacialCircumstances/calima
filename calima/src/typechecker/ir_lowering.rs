use crate::ir::{Binding, Block, Val};

#[derive(Debug, Clone)]
struct PartialBlock(Vec<Binding>);

impl PartialBlock {
    pub fn new() -> Self {
        PartialBlock(Vec::new())
    }

    pub fn end(self, value: Val) -> Block {
        Block(self.0, value)
    }
}

pub struct BlockBuilder {
    blocks: Vec<PartialBlock>,
}

impl BlockBuilder {
    pub fn new() -> Self {
        Self { blocks: Vec::new() }
    }

    pub fn begin(&mut self) {
        self.blocks.push(PartialBlock::new())
    }

    pub fn add_binding(&mut self, binding: Binding) {
        self.blocks
            .last_mut()
            .expect("Block stack may not be empty")
            .0
            .push(binding)
    }

    pub fn end(&mut self, value: Val) -> Block {
        self.blocks
            .pop()
            .expect("Block stack may not be empty")
            .end(value)
    }
}
