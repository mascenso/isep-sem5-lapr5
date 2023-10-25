import { Result } from "../../core/logic/Result";
import IRobotDTO from "../../dto/IRobotDTO";

export default interface IRobotService  {
  createRobot(robotDTO: IRobotDTO): Promise<Result<IRobotDTO>>;
  updateRobot(robotDTO: IRobotDTO): Promise<Result<IRobotDTO>>;
  getAllRobots(robotDTO: IRobotDTO): Promise<Result<IRobotDTO >>;
  findByDesignationOrTaskType(designation: string, taskType: string): Promise<Result<IRobotDTO[]>>;
}
