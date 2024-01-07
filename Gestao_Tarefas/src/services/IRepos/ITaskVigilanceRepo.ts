import { Repo } from "../../core/infra/Repo";
import { TaskVigilance } from "../../domain/task-agg/TaskVigilance";
import { TaskVigilanceId } from "../../domain/task-agg/taskVigilanceId";
import {TaskPickupDelivery} from "../../domain/task-agg/TaskPickupDelivery";
import {TaskStatus} from "../../domain/task-agg/TaskStatus";

export default interface ITaskVigilanceRepo extends Repo<TaskVigilance> {
  save(task: TaskVigilance): Promise<TaskVigilance>;
  findByDomainId (taskId: TaskVigilanceId | string): Promise<TaskVigilance>;

  //findByIds (tasksIds: TaskId[]): Promise<Task[]>;
  //saveCollection (tasks: Task[]): Promise<Task[]>;
  //removeByTaskIds (tasks: TaskId[]): Promise<any>
  findAll(): Promise<TaskVigilance[]>;
  findByUserEmail(userEmail: string): Promise<TaskVigilance[]>;
  findByTaskStatus(status: TaskStatus): Promise<TaskVigilance[]>;

}
