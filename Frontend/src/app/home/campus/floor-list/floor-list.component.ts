import {Component, OnInit} from '@angular/core';
import {FormControl} from "@angular/forms";
import {BuildingResponseDto, BuildingService} from "../../../services/building.service";

@Component({
  selector: 'app-floor-list',
  templateUrl: './floor-list.component.html',
  styleUrls: ['./floor-list.component.css']
})
export class FloorListComponent implements OnInit {

  buildingList = [
    {
      id: 'random-id-1',
      code: 'code1'
    },
    {
      id: 'random-id-2',
      code: 'code2'
    },
    {
      id: 'random-id-3',
      code: 'code3'
    },
    {
      id: 'random-id-4',
      code: 'code4'
    }
  ];

  buildingSelectionControl =  new FormControl();

  constructor(private buildingService: BuildingService) {}

  ngOnInit(): void {
    // fetch building list from service
  }

}
