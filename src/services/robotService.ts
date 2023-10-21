import { Service, Inject } from 'typedi';
import config from "../../config";
import IRobotDTO from '../dto/IRobotDTO';
import { Robot } from "../domain/robot";
import IRobotRepo from './IRepos/IRobotRepo';
import IRobotService from './IServices/IRobotService';
import { Result } from "../core/logic/Result";
import { RobotMap } from "../mappers/RobotMap";

@Service()
export default class RobotService implements IRobotService {
  constructor(
      @Inject(config.repos.robot.name) private robotRepo : IRobotRepo
  ) {}

  public async getRobot( robotId: string): Promise<Result<IRobotDTO>> {
    try {
      const robot = await this.robotRepo.findByDomainId(robotId);

      if (robot === null) {
        return Result.fail<IRobotDTO>("Robot not found");
      }
      else {
        const robotDTOResult = RobotMap.toDTO( robot ) as IRobotDTO;
        return Result.ok<IRobotDTO>( robotDTOResult )
        }
    } catch (e) {
      throw e;
    }
  }


  public async createRobot(robotDTO: IRobotDTO): Promise<Result<IRobotDTO>> {
    try {

      const robotOrError = await Robot.create( robotDTO );

      if (robotOrError.isFailure) {
        return Result.fail<IRobotDTO>(robotOrError.errorValue());
      }

      const robotResult = robotOrError.getValue();

      await this.robotRepo.save(robotResult);

      const robotDTOResult = RobotMap.toDTO( robotResult ) as IRobotDTO;
      return Result.ok<IRobotDTO>( robotDTOResult )
    } catch (e) {
      throw e;
    }
  }

  public async updateRobot(robotDTO: IRobotDTO): Promise<Result<IRobotDTO>> {
    try {
      const robot = await this.robotRepo.findByDomainId(robotDTO.id);

      if (robot === null) {
        return Result.fail<IRobotDTO>("Robot not found");
      }
      else {
        robot.designacao = robotDTO.designacao;
        robot.tarefas = robotDTO.tarefas;
        await this.robotRepo.save(robot);

        const robotDTOResult = RobotMap.toDTO( robot ) as IRobotDTO;
        return Result.ok<IRobotDTO>( robotDTOResult )
        }
    } catch (e) {
      throw e;
    }
  }

}
