import { Mapper } from "../core/infra/Mapper";

import { UniqueEntityID } from "../core/domain/UniqueEntityID";

import { TaskVigilance } from "../domain/task-agg/TaskVigilance";
import ITaskVigilanceDTO from "../dto/ITaskVigilanceDTO";
import {Document, Model} from "mongoose";
import {ITaskVigilancePersistence} from "../dataschema/ITaskVigilancePersistence";
import {TaskPickupDelivery} from "../domain/task-agg/TaskPickupDelivery";
import ITaskSearchResponseDTO, {TaskType} from "../dto/ITaskSearchResponseDTO";
import { TaskStatusVO } from "../domain/task-agg/taskStatusVO";

export class TaskVigilanceMap extends Mapper<TaskVigilance> {

    public static toDTO( taskVigilance: TaskVigilance): ITaskVigilanceDTO {

      return {
        id: taskVigilance.id.toString(),
        description: taskVigilance.description,
        buildingId: taskVigilance.buildingId,
        floors: taskVigilance.floors,
        startPosition: taskVigilance.startPosition,
        endPosition: taskVigilance.endPosition,
        contactNumber: taskVigilance.contactNumber,
        user: taskVigilance.user,
        taskStatus: taskVigilance.taskStatus,
      } as ITaskVigilanceDTO;
    }

    public static toDomain (taskVigilance: any | Model<ITaskVigilancePersistence & Document> ): TaskVigilance {

      const taskOrError = TaskVigilance.create({
            description: taskVigilance.description,
            buildingId: taskVigilance.buildingId,
            floors: taskVigilance.floors,
            startPosition: taskVigilance.startPosition,
            endPosition: taskVigilance.endPosition,
            contactNumber: taskVigilance.contactNumber,
            user: taskVigilance.user,
            taskStatus: taskVigilance.taskStatus
        },
        new UniqueEntityID(taskVigilance.domainId)
      );

      taskOrError.isFailure ? console.log(taskOrError.error) : '';

      return taskOrError.isSuccess ? taskOrError.getValue() : null;
    }

    public static toPersistence (taskVigilance: TaskVigilance): any {
      const a = {
        domainId: taskVigilance.id.toString(),
        description: taskVigilance.description,
        buildingId: taskVigilance.buildingId,
        floors: taskVigilance.floors,
        startPosition: taskVigilance.startPosition,
        endPosition: taskVigilance.endPosition,
        contactNumber: taskVigilance.contactNumber,
        user: taskVigilance.user,
        taskStatus: taskVigilance.taskStatus
      }
      return a;
    }

  public static toSearchResponseDTO( taskVigilance: TaskVigilance): ITaskSearchResponseDTO {

    return {
      id: taskVigilance.id.toString(),
      description: taskVigilance.description,
      buildingId: taskVigilance.buildingId,
      floors: taskVigilance.floors,
      startPosition: taskVigilance.startPosition,
      endPosition: taskVigilance.endPosition,
      contactNumber: taskVigilance.contactNumber,
      user: taskVigilance.user,
      taskStatus: taskVigilance.taskStatus,
      taskType: TaskType.VIGILANCE,
    } as ITaskSearchResponseDTO;
  }


}
