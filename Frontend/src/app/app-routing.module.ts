import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import {HomeComponent} from "./home/home.component";
import {CampusComponent} from "./home/campus/campus.component";
import {FleetComponent} from "./home/fleet/fleet.component";
import {TasksComponent} from "./home/tasks/tasks.component";
import {LoginComponent} from "./login/login.component";
import {CreateBuildingComponent} from "./home/campus/create-building/create-building.component";

const routes: Routes = [
  {path: '', redirectTo: 'home', pathMatch:'full'},
  {
    path: 'home', component: HomeComponent,
    children: [
      {
        path: 'campus',
        component: CampusComponent
      },
      {
        path: 'fleet',
        component: FleetComponent,
      },
      {
        path: 'tasks',
        component: TasksComponent,
      },
      {
        path: 'campus/create-building',
        component: CreateBuildingComponent,
      },
    ],
  },
  { path: '**', redirectTo: ''}
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
