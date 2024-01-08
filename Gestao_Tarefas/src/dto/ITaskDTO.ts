import { TaskStatus } from "../domain/task-agg/TaskStatus";

export default interface ITaskDTO {
  id: string;
  taskStatus:string;
}
