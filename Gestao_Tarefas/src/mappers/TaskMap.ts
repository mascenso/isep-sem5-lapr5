import { Mapper } from "../core/infra/Mapper";

import { Document, Model } from 'mongoose';
import { ITaskPersistence } from '../dataschema/ITaskPersistence';

import ITaskDTO from "../dto/ITaskDTO";
import { Task } from "../domain/task";

import { UniqueEntityID } from "../core/domain/UniqueEntityID";

export class TaskMap extends Mapper<Task> {

  public static toDTO( task: Task): ITaskDTO {
    return {
      id: task.id.toString(),
      name: task.name,
    } as ITaskDTO;
  }

  public static toDomain (task: any | Model<ITaskPersistence & Document> ): Task {
    const taskOrError = Task.create(
      task,
      new UniqueEntityID(task.domainId)
    );

    taskOrError.isFailure ? console.log(taskOrError.error) : '';

    return taskOrError.isSuccess ? taskOrError.getValue() : null;
  }

  public static toPersistence (task: Task): any {
    return {
      domainId: task.id.toString(),
      name: task.name
    }
  }
}
