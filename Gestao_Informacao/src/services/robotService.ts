import { Service, Inject } from 'typedi';
import config from "../../config";
import IRobotDTO from '../dto/IRobotDTO';
import { Robot } from "../domain/robot-agg/robot";
import IRobotRepo from './IRepos/IRobotRepo';
import IRobotService from './IServices/IRobotService';
import { Result } from "../core/logic/Result";
import { RobotMap } from "../mappers/RobotMap";
import IRobotTypeRepo from "./IRepos/IRobotTypeRepo";
import {RobotType} from "../domain/robotType-agg/robotType";

@Service()
export default class RobotService implements IRobotService {
  constructor(
      @Inject(config.repos.robotType.name) private robotTypeRepo : IRobotTypeRepo,
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

      /* check if robot type exists */
      const robotTypeId = await this.robotTypeRepo.findByDomainId(robotDTO.robotType);
      if (robotTypeId === null) {
        return Result.fail<IRobotDTO>('Robot type not found');
      }

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

        robot.inhibited = robotDTO.inhibited;

        await this.robotRepo.save(robot);

        const robotDTOResult = RobotMap.toDTO( robot ) as IRobotDTO;
        return Result.ok<IRobotDTO>( robotDTOResult )
        }
    } catch (e) {
      throw e;
    }
  }

  public async getAllRobots(): Promise<Result<IRobotDTO[]>> {
    try {

      const robots = await this.robotRepo.getAllRobots();

      const robotDTOs = robots.map((robots) => RobotMap.toDTO(robots) as IRobotDTO);

      return Result.ok<IRobotDTO[]>(robotDTOs);


    } catch (e) {
      throw e;
    }
  }

  public async findByDesignationOrTaskType(designation: string, taskType: string): Promise<Result<IRobotDTO[]>> {
    try {
      // if no criteria is provided return all robots
      if ((taskType === '' || undefined) && (designation === '' || undefined)) {
        return this.getAllRobots();
      }

      // find robotTypes that include the taskType
      let robotTypesList: RobotType[] = [];
      if (taskType) {
        robotTypesList = await this.robotTypeRepo.findByTaskType(taskType);
      }

      const robotList = await this.robotRepo.findByDesignationOrTaskType(
                      designation,
                      robotTypesList.flatMap(robotType => robotType.id.toString())
      );

      if (robotList.length === 0) {
        return Result.fail<IRobotDTO[]>(`Unable to find Robots that perform ${taskType} or designated by ${designation}`);

      }

      const robotDTOList = robotList.map(robot => RobotMap.toDTO(robot) as IRobotDTO);
      return Result.ok<IRobotDTO[]>(robotDTOList);

    } catch (e) {
      throw e;
    }
  }

}
