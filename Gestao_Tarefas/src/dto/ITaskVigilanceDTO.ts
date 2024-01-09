import { TaskStatusVO } from "../domain/task-agg/taskStatusVO";

export default interface ITaskVigilanceDTO {
  id: string;
  description: string;
  buildingId: string;
  floors: object[];
  startPosition: number[];
  endPosition: number[];
  contactNumber: number;
  user:object;
  taskStatus: TaskStatusVO;
}
