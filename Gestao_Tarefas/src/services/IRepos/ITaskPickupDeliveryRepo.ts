import { Repo } from "../../core/infra/Repo";
import { TaskPickupDelivery } from "../../domain/task-agg/taskPickupDelivery";
import { TaskPickupDeliveryId } from "../../domain/task-agg/taskPickupDeliveryId";

export default interface ITaskPickupDeliveryRepo extends Repo<TaskPickupDelivery> {
  save(task: TaskPickupDelivery): Promise<TaskPickupDelivery>;
  findByDomainId (taskId: TaskPickupDeliveryId | string): Promise<TaskPickupDelivery>;

  //findByIds (tasksIds: TaskId[]): Promise<Task[]>;
  //saveCollection (tasks: Task[]): Promise<Task[]>;
  //removeByTaskIds (tasks: TaskId[]): Promise<any>

  findAll(): Promise<TaskPickupDelivery[]>;
}