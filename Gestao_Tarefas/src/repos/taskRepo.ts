import { Service, Inject } from 'typedi';

import ITaskRepo from "../services/IRepos/ITaskRepo";
import { Task } from "../domain/task";
import { TaskId } from "../domain/taskId";
import { TaskMap } from "../mappers/TaskMap";

import { Document, FilterQuery, Model } from 'mongoose';
import { ITaskPersistence } from '../dataschema/ITaskPersistence';

@Service()
export default class TaskRepo implements ITaskRepo {
  private models: any;

  constructor(
    @Inject('taskSchema') private taskSchema : Model<ITaskPersistence & Document>,
  ) {}

  private createBaseQuery (): any {
    return {
      where: {},
    }
  }

  public async exists(task: Task): Promise<boolean> {

    const idX = task.id instanceof TaskId ? (<TaskId>task.id).toValue() : task.id;

    const query = { domainId: idX};
    const taskDocument = await this.taskSchema.findOne( query as FilterQuery<ITaskPersistence & Document>);

    return !!taskDocument === true;
  }

  public async save (task: Task): Promise<Task> {
    const query = { domainId: task.id.toString()};

    const taskDocument = await this.taskSchema.findOne( query );

    try {
      if (taskDocument === null ) {
        const rawTask: any = TaskMap.toPersistence(task);

        const taskCreated = await this.taskSchema.create(rawTask);

        return TaskMap.toDomain(taskCreated);
      } else {
        taskDocument.name = task.name;
        await taskDocument.save();

        return task;
      }
    } catch (err) {
      throw err;
    }
  }

  public async findByDomainId (taskId: TaskId | string): Promise<Task> {
    const query = { domainId: taskId};
    const taskRecord = await this.taskSchema.findOne( query as FilterQuery<ITaskPersistence & Document> );

    if( taskRecord != null) {
      return TaskMap.toDomain(taskRecord);
    }
    else
      return null;
  }

  public async findAll(): Promise<Task[]> {
    const tasks = await this.taskSchema.find();
    return tasks.map((task) => TaskMap.toDomain(task));
  }
}
