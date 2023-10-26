import { Repo } from "../../core/infra/Repo";
import { RobotType } from "../../domain/robotType";
import { RobotTypeId } from "../../domain/robotTypeId";

export default interface IRobotRepo extends Repo<RobotType> {
  save(robotType: RobotType): Promise<RobotType>;
  findByDomainId (robotTypeId: RobotTypeId | string): Promise<RobotType>;
  findByDesignationOrTaskType(designation: string, taskType: string): Promise<RobotType[]>;
}