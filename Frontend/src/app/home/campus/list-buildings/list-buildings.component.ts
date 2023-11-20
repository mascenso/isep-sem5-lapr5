import { Component, OnInit } from '@angular/core';
import {BuildingService} from '../../../services/building.service'
import { BuildingResponseDTO } from "../../../../dto/buildingDTO";

@Component({
  selector: 'app-list-buildings',
  templateUrl: './list-buildings.component.html',
  styleUrls: ['./list-buildings.component.css']
})
export class ListBuildingsComponent implements OnInit{
  edificios: BuildingResponseDTO[] = [];
  displayedColumns: string[] = ['code', 'name', 'description', 'maxLength', 'maxWidth'];

  constructor(private buildingService: BuildingService) { }

  ngOnInit(): void {
    this.carregarEdificios();
  }

  carregarEdificios() {
    this.buildingService.getAllBuildings().subscribe(edificios => {
      console.log(edificios)
      this.edificios = edificios;
      console.log(this.edificios)
    });
    
  }
}
