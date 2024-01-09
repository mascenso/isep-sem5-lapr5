import { TaskStatus } from "../domain/task-agg/TaskStatus";

export default interface ITaskPatchRequestDTO {
  id: string;
  taskStatus:string;
}
