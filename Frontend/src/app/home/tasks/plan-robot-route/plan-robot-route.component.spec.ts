import { ComponentFixture, TestBed } from '@angular/core/testing';

import { PlanRobotRouteComponent } from './plan-robot-route.component';

describe('PlanRobotRouteComponent', () => {
  let component: PlanRobotRouteComponent;
  let fixture: ComponentFixture<PlanRobotRouteComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [PlanRobotRouteComponent]
    });
    fixture = TestBed.createComponent(PlanRobotRouteComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
