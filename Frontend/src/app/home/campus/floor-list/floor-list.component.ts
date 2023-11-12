import {Component, OnInit} from '@angular/core';
import {FormControl} from "@angular/forms";
import { BuildingService } from "../../../services/building.service";
import {animate, state, style, transition, trigger} from "@angular/animations";

@Component({
  selector: 'app-floor-list',
  templateUrl: './floor-list.component.html',
  animations: [
    trigger('detailExpand', [
      state('collapsed,void', style({height: '0px', minHeight: '0'})),
      state('expanded', style({height: '*'})),
      transition('expanded <=> collapsed', animate('225ms cubic-bezier(0.4, 0.0, 0.2, 1)')),
    ]),
  ],
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

  dataSource = FLOOR_DATA;
  columnsToDisplay = ['id', 'floorNumber', 'width', 'length'];
  columnsToDisplayWithExpand = [...this.columnsToDisplay, 'expand'];
  expandedElement: FloorDTO | null | undefined;

  buildingSelectionControl =  new FormControl();

  constructor(private buildingService: BuildingService) {}

  ngOnInit(): void {
    // fetch building list from service
  }

  onSelectionUpdateTable(selection: any) {

  }

}

export interface FloorDTO {
  id: string;
  buildingId:string;
  width: number;
  length: number;
  floorNumber: number;
  description: string;
  floorMap: number[][];
}

const FLOOR_DATA: FloorDTO[] = [
  {
    id: 'floor-id-1',
    buildingId: 'building-id-1',
    width: 10,
    length: 10,
    floorNumber: 1,
    description: `Hydrogen is a chemical element with symbol H and atomic number 1. With a standard
        atomic weight of 1.008, hydrogen is the lightest element on the periodic table.`,
    floorMap: [[0,1,0,1,1],[0,0,0,0,0],[0,1,0,1,1],[0,1,0,1,1],[0,1,0,1,1]]
  },
  {
    id: 'floor-id-2',
    buildingId: 'building-id-2',
    width: 10,
    length: 10,
    floorNumber: 2,
    description: `Hydrogen is a chemical element with symbol H and atomic number 1. With a standard
        atomic weight of 1.008, hydrogen is the lightest element on the periodic table.`,
    floorMap: [[0,1,0,1,1],[0,0,0,0,0],[0,1,0,1,1],[0,1,0,1,1],[0,1,0,1,1]]
  },
  {
    id: 'floor-id-3',
    buildingId: 'building-id-2',
    width: 10,
    length: 10,
    floorNumber: 3,
    description: `Hydrogen is a chemical element with symbol H and atomic number 1. With a standard
        atomic weight of 1.008, hydrogen is the lightest element on the periodic table.`,
    floorMap: [[0,1,0,1,1],[0,0,0,0,0],[0,1,0,1,1],[0,1,0,1,1],[0,1,0,1,1]]
  },
];
