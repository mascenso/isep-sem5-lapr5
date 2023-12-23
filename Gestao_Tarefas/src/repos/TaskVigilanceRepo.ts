import { Service, Inject } from 'typedi';

import { TaskVigilanceId } from "../domain/task-agg/taskVigilanceId";
import { TaskVigilanceMap } from "../mappers/TaskVigilanceMap";

import { Document, FilterQuery, Model } from 'mongoose';
import { ITaskVigilancePersistence } from '../dataschema/ITaskVigilancePersistence';
import { TaskVigilance } from '../domain/task-agg/TaskVigilance';
import ITaskVigilanceRepo from '../services/IRepos/ITaskVigilanceRepo';

@Service()
export default class TaskVigilanceRepo implements ITaskVigilanceRepo {
  private models: any;

  constructor(
    @Inject('taskVigilanceSchema') private taskVigilanceSchema : Model<ITaskVigilancePersistence & Document>,
  ) {}

  private createBaseQuery (): any {
    return {
      where: {},
    }
  }

  public async exists(taskVigilance: TaskVigilance): Promise<boolean> {

    const idX = taskVigilance.id instanceof TaskVigilanceId ? (<TaskVigilanceId>taskVigilance.id).toValue() : taskVigilance.id;

    const query = { domainId: idX};
    const taskDocument = await this.taskVigilanceSchema.findOne( query as FilterQuery<ITaskVigilancePersistence & Document>);

    return !!taskDocument === true;
  }

  public async save (taskVigilance: TaskVigilance): Promise<TaskVigilance> {
    const query = { domainId: taskVigilance.id.toString()};

    const taskDocument = await this.taskVigilanceSchema.findOne( query );

    try {
      if (taskDocument === null ) {
        const rawTask: any = TaskVigilanceMap.toPersistence(taskVigilance);

        const taskCreated = await this.taskVigilanceSchema.create(rawTask);

        return TaskVigilanceMap.toDomain(taskCreated);
      } else {
        taskDocument.id = taskVigilance.id;
        await taskDocument.save();

        return taskVigilance;
      }
    } catch (err) {
      throw err;
    }
  }

  public async findByDomainId (taskId: TaskVigilanceId | string): Promise<TaskVigilance> {
    const query = { domainId: taskId};
    const taskRecord = await this.taskVigilanceSchema.findOne( query as FilterQuery<ITaskVigilancePersistence & Document> );

    if( taskRecord != null) {
      return TaskVigilanceMap.toDomain(taskRecord);
    }
    else
      return null;
  }

  public async findAll(): Promise<TaskVigilance[]> {
    const tasks = await this.taskVigilanceSchema.find();
    return tasks.map((task) => TaskVigilanceMap.toDomain(task));
  }

}

