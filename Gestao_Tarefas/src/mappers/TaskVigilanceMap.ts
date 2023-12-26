import { Mapper } from "../core/infra/Mapper";

import { UniqueEntityID } from "../core/domain/UniqueEntityID";

import {TaskVigilance} from "../domain/task-agg/taskVigilance";
import ITaskVigilanceDTO from "../dto/ITaskVigilanceDTO";
import {Document, Model} from "mongoose";
import {ITaskVigilancePersistence} from "../dataschema/ITaskVigilancePersistence";

export class TaskVigilanceMap extends Mapper<TaskVigilance> {

    public static toDTO( taskVigilance: TaskVigilance): ITaskVigilanceDTO {
  
      return {
        id: taskVigilance.id.toString(),
        description: taskVigilance.description,
        buildingId: taskVigilance.buildingId,
        floors: taskVigilance.floors,
        contactNumber: taskVigilance.contactNumber,
        user: taskVigilance.user,
        approved: taskVigilance.approved,
        pending: taskVigilance.pending,
        planned: taskVigilance.planned
      } as ITaskVigilanceDTO;
    }
  
    public static toDomain (taskVigilance: any | Model<ITaskVigilancePersistence & Document> ): TaskVigilance {
  
      const taskOrError = TaskVigilance.create({
            description: taskVigilance.description,
            buildingId: taskVigilance.buildingId,
            floors: taskVigilance.floors,
            contactNumber: taskVigilance.contactNumber,
            user: taskVigilance.user,
            approved: taskVigilance.approved,
            pending: taskVigilance.pending,
            planned: taskVigilance.planned
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
        contactNumber: taskVigilance.contactNumber,
        user: taskVigilance.user,
        approved: taskVigilance.approved,
        pending: taskVigilance.pending,
        planned: taskVigilance.planned
      }
      return a;
    }
  }
  