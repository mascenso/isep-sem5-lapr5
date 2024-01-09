import { Result } from "../../core/logic/Result";
import ITaskPatchRequestDTO from "../../dto/ITaskPatchRequestDTO";
import ITaskPickupDeliveryDTO from "../../dto/ITaskPickupDeliveryDTO";
import ITaskVigilanceDTO from "../../dto/ITaskVigilanceDTO";
import ITaskSearchResponseDTO from "../../dto/ITaskSearchResponseDTO";

export default interface ITaskService  {
  updateTask(taskDTO: ITaskPatchRequestDTO, taskId: string): Promise<Result<ITaskPickupDeliveryDTO | ITaskVigilanceDTO>>;
  createVigilanceTask(taskDTO: ITaskVigilanceDTO): Promise<Result<ITaskVigilanceDTO>>;
  createPickupDeliveryTask(taskDTO: ITaskPickupDeliveryDTO): Promise<Result<ITaskPickupDeliveryDTO>>;
  getAllVigilancePendingTasks(): Promise<Result<Array<ITaskVigilanceDTO[]>>>;
  getAllPickupDeliveryPendingTasks(): Promise<Result<Array<ITaskPickupDeliveryDTO[]>>>;
  getAllPendingTasks():Promise<Result<Array<any[]>>>;
  getAllApprovedTasks():Promise<Result<Array<any[]>>>;
  getTasksPlanning(params:any):Promise<Result<any>>;
  getAllPickupDeliveryApprovedTasks(): Promise<Result<Array<ITaskPickupDeliveryDTO[]>>>;
  getAllVigilanceApprovedTasks(): Promise<Result<Array<ITaskVigilanceDTO[]>>>;
  getTasksByUserEmail(userEmail: string): Promise<Result<Array<ITaskSearchResponseDTO>>>;
  getTasksByStatus(status: string): Promise<Result<Array<ITaskSearchResponseDTO>>>;
}
