import { Result } from "../../core/logic/Result";
import IRoleDTO from "../../dto/IRoleDTO";

export default interface IRobotService  {
  createRobot(robotDTO: IRobotDTO): Promise<Result<IRobotDTO>>;
  updateRobot(robotDTO: IRobotDTO): Promise<Result<IRobotDTO>>;

  getRobot (robotId: string): Promise<Result<IRobotDTO>>;
}
