import { Component, OnInit } from '@angular/core';
import { RobotService } from 'src/app/services/robot.service';
import { RobotDTO } from 'src/dto/robotDTO';

@Component({
  selector: 'app-fleet-list',
  templateUrl: './fleet-list.component.html',
  styleUrls: ['./fleet-list.component.css']
})
export class FleetListComponent implements OnInit{

  robot: RobotDTO[] = [];
  displayedColumns: string[] = [ 'nickName', 'robotType', 'serialNumber', 'inhibited'];

  constructor(private robotService: RobotService) { }

  ngOnInit(): void {
    this.carregarFrota();
  }

  carregarFrota() {
    this.robotService.getAllRobots().subscribe(robot => {
      this.robot = robot;
    });
    
  }

}
