import type { Task } from "@/types";

/**
 * A task with its nested children, forming a tree structure.
 */
export interface TaskNode {
  task: Task;
  children: TaskNode[];
}

/**
 * Build a tree from a flat DFS-ordered task list using taskNum/parentTaskNum.
 * Top-level tasks (parentTaskNum === undefined) become roots.
 */
export function buildTaskTree(tasks: Task[]): TaskNode[] {
  const nodeMap = new Map<number, TaskNode>();
  const roots: TaskNode[] = [];

  // Create nodes
  for (const task of tasks) {
    const node: TaskNode = { task, children: [] };
    nodeMap.set(task.taskNum, node);

    if (task.parentTaskNum != null) {
      const parent = nodeMap.get(task.parentTaskNum);
      if (parent) {
        parent.children.push(node);
      } else {
        // Parent not found (filtered out?), treat as root
        roots.push(node);
      }
    } else {
      roots.push(node);
    }
  }

  return roots;
}
