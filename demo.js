import React, { useState, useEffect } from 'react';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { RotateCcw } from 'lucide-react';

// Tree node for visualization
const TreeNode = ({ value, x, y, isHighlighted, isComparing, isInPath, className }) => (
  <div 
    className={`absolute transform -translate-x-1/2 -translate-y-1/2 w-12 h-12 rounded-full 
    flex items-center justify-center transition-all duration-500 
    ${isHighlighted ? 'bg-yellow-200' : isComparing ? 'bg-green-200' : isInPath ? 'bg-blue-200' : 'bg-blue-100'}
    ${className}`}
    style={{ left: x, top: y }}
  >
    <span className="text-sm font-medium">{value}</span>
  </div>
);

// Edge component to connect nodes
const Edge = ({ x1, y1, x2, y2, isInPath }) => (
  <line
    x1={x1}
    y1={y1}
    x2={x2}
    y2={y2}
    stroke={isInPath ? "blue" : "gray"}
    strokeWidth={isInPath ? "3" : "2"}
    className="transition-all duration-500"
  />
);

const HeapVisualizer = () => {
  const [heap, setHeap] = useState([]);
  const [highlightedNode, setHighlightedNode] = useState(null);
  const [comparingNodes, setComparingNodes] = useState([]);
  const [insertPath, setInsertPath] = useState([]);
  const [isAnimating, setIsAnimating] = useState(false);
  const [newValue, setNewValue] = useState('');

  const sleep = (ms) => new Promise(resolve => setTimeout(resolve, ms));

  // Calculate the path to the insertion point (similar to find_path_to_last)
  const findPathToLast = (size) => {
    const path = [];
    let current = size;
    while (current > 1) {
      path.unshift(current % 2 === 0 ? 'L' : 'R');
      current = Math.floor(current / 2);
    }
    return path;
  };

  // Convert path to array indices
  const pathToIndices = (path) => {
    const indices = [0]; // Start from root
    let currentIndex = 0;
    for (const direction of path) {
      if (direction === 'L') {
        currentIndex = 2 * currentIndex + 1;
      } else {
        currentIndex = 2 * currentIndex + 2;
      }
      indices.push(currentIndex);
    }
    return indices;
  };

  const insertNode = async (value) => {
    setIsAnimating(true);
    const newHeap = [...heap];
    const insertSize = newHeap.length + 1;
    const path = findPathToLast(insertSize);
    const pathIndices = pathToIndices(path);
    
    setInsertPath(pathIndices);
    await sleep(1000);

    // Recursive insertion following the path
    const insertRecursive = async (value, currentPathIndices) => {
      if (currentPathIndices.length === 0) {
        return value;
      }

      const currentIndex = currentPathIndices[0];
      
      // Highlight current node and value being inserted
      setComparingNodes([currentIndex]);
      setHighlightedNode(-1); // Use -1 to show the value being inserted
      await sleep(1000);

      if (currentIndex >= newHeap.length) {
        newHeap.push(value);
        setHeap([...newHeap]);
        return null;
      }

      // Compare current node with value to insert
      if (value < newHeap[currentIndex]) {
        // If new value is smaller, swap and continue with old value
        const temp = newHeap[currentIndex];
        newHeap[currentIndex] = value;
        setHeap([...newHeap]);
        await sleep(1000);
        
        return insertRecursive(temp, currentPathIndices.slice(1));
      } else {
        // Continue with new value
        return insertRecursive(value, currentPathIndices.slice(1));
      }
    };

    await insertRecursive(value, pathIndices);
    
    // Clear highlights
    setInsertPath([]);
    setComparingNodes([]);
    setHighlightedNode(null);
    setIsAnimating(false);
  };

  const removeMin = async () => {
    if (heap.length === 0) return;
    
    setIsAnimating(true);
    
    const newHeap = [...heap];
    setHighlightedNode(0);
    await sleep(1000);
    
    // Remove root and get last element
    const lastElement = newHeap.pop();
    if (newHeap.length > 0) {
      newHeap[0] = lastElement;
    }
    setHeap([...newHeap]);
    
    // Heapify down
    let currentIndex = 0;
    while (true) {
      const leftChild = 2 * currentIndex + 1;
      const rightChild = 2 * currentIndex + 2;
      let smallest = currentIndex;
      
      setComparingNodes([currentIndex, leftChild, rightChild].filter(i => i < newHeap.length));
      await sleep(500);
      
      if (leftChild < newHeap.length && newHeap[leftChild] < newHeap[smallest]) {
        smallest = leftChild;
      }
      if (rightChild < newHeap.length && newHeap[rightChild] < newHeap[smallest]) {
        smallest = rightChild;
      }
      
      if (smallest !== currentIndex) {
        [newHeap[currentIndex], newHeap[smallest]] = 
        [newHeap[smallest], newHeap[currentIndex]];
        setHeap([...newHeap]);
        
        await sleep(500);
        currentIndex = smallest;
      } else {
        break;
      }
    }
    
    setHighlightedNode(null);
    setComparingNodes([]);
    setIsAnimating(false);
  };

  // Calculate node positions
  const getNodePosition = (index) => {
    const level = Math.floor(Math.log2(index + 1));
    const nodesInLevel = Math.pow(2, level);
    const positionInLevel = index - (Math.pow(2, level) - 1);
    const width = 600;
    const height = 400;
    const levelHeight = height / 4;
    
    const x = width * (positionInLevel + 1) / (nodesInLevel + 1);
    const y = (level + 1) * levelHeight;
    
    return { x, y };
  };

  const reset = () => {
    setHeap([]);
    setHighlightedNode(null);
    setComparingNodes([]);
    setInsertPath([]);
    setIsAnimating(false);
    setNewValue('');
  };

  return (
    <Card className="w-full max-w-4xl">
      <CardHeader>
        <CardTitle>Binary Min Heap Visualization</CardTitle>
      </CardHeader>
      <CardContent>
        <div className="flex gap-4 mb-4">
          <input
            type="number"
            value={newValue}
            onChange={(e) => setNewValue(e.target.value)}
            className="border rounded px-2 py-1 w-24"
            placeholder="Value"
            disabled={isAnimating}
          />
          <Button 
            onClick={() => {
              if (newValue) {
                insertNode(parseInt(newValue));
                setNewValue('');
              }
            }}
            disabled={isAnimating || !newValue}
          >
            Insert
          </Button>
          <Button 
            onClick={removeMin}
            disabled={isAnimating || heap.length === 0}
          >
            Remove Min
          </Button>
          <Button 
            onClick={reset}
            disabled={isAnimating}
          >
            <RotateCcw className="w-4 h-4 mr-2" />
            Reset
          </Button>
        </div>
        
        <div className="relative w-[600px] h-[400px] border rounded-lg">
          <svg className="absolute w-full h-full">
            {heap.map((_, index) => {
              if (index === 0) return null;
              const parentIndex = Math.floor((index - 1) / 2);
              const { x: x1, y: y1 } = getNodePosition(parentIndex);
              const { x: x2, y: y2 } = getNodePosition(index);
              return (
                <Edge 
                  key={`edge-${index}`} 
                  x1={x1} 
                  y1={y1} 
                  x2={x2} 
                  y2={y2} 
                  isInPath={insertPath.includes(index) && insertPath.includes(parentIndex)}
                />
              );
            })}
          </svg>
          
          {heap.map((value, index) => {
            const { x, y } = getNodePosition(index);
            return (
              <TreeNode
                key={`node-${index}`}
                value={value}
                x={x}
                y={y}
                isHighlighted={highlightedNode === index}
                isComparing={comparingNodes.includes(index)}
                isInPath={insertPath.includes(index)}
              />
            );
          })}
        </div>
      </CardContent>
    </Card>
  );
};

export default HeapVisualizer;