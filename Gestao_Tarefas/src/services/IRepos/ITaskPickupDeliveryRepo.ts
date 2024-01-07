import { Repo } from "../../core/infra/Repo";
import { TaskPickupDelivery } from "../../domain/task-agg/TaskPickupDelivery";
import { TaskPickupDeliveryId } from "../../domain/task-agg/TaskPickupDeliveryId";
import {TaskStatus} from "../../domain/task-agg/TaskStatus";


export default interface ITaskPickupDeliveryRepo extends Repo<TaskPickupDelivery> {
  save(task: TaskPickupDelivery): Promise<TaskPickupDelivery>;
  findByDomainId (taskId: TaskPickupDeliveryId | string): Promise<TaskPickupDelivery>;

  //findByIds (tasksIds: TaskId[]): Promise<Task[]>;
  //saveCollection (tasks: Task[]): Promise<Task[]>;
  //removeByTaskIds (tasks: TaskId[]): Promise<any>

  findAll(): Promise<TaskPickupDelivery[]>;

  findByUserEmail(userEmail: string): Promise<TaskPickupDelivery[]>;
  findByTaskStatus(status: TaskStatus): Promise<TaskPickupDelivery[]>;
}
