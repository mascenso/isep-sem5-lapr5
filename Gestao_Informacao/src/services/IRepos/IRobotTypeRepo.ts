import { Repo } from "../../core/infra/Repo";
import { RobotType } from "../../domain/robotType-agg/robotType";
import { RobotTypeId } from "../../domain/robotType-agg/robotTypeId";
import TaskType from "../../enums/taskType";

export default interface IRobotTypeRepo extends Repo<RobotType> {
  save(robotType: RobotType): Promise<RobotType>;
  findByDomainId (robotTypeId: RobotTypeId | string): Promise<RobotType>;
  findByDesignationOrTaskType(designation: string, taskType: TaskType[]): Promise<RobotType[]>;
}
