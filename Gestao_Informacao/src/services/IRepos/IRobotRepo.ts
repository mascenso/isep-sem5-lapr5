import { Repo } from "../../core/infra/Repo";
import { Robot } from "../../domain/robot-agg/robot";
import { RobotId } from "../../domain/robot-agg/robotId";

export default interface IRobotRepo extends Repo<Robot> {
  save(robot: Robot): Promise<Robot>;
  findByDomainId (robotId: RobotId | string): Promise<Robot>;
  getAllRobots (): Promise<Robot[]>;
  findByDesignationOrTaskType(designation: string, robotTypeList: string[]): Promise<Robot[]>;
}
