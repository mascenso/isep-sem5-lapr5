import 'reflect-metadata';

import * as sinon from 'sinon';
import { Response, Request, NextFunction } from 'express';
import { Container } from 'typedi';
import { Result } from '../../src/core/logic/Result';
import IFloorService from "../../src/services/IServices/IFloorService";
import FloorController from "../../src/controllers/floorController";
import {IBuildingDTO} from '../../src/dto/IBuildingDTO';
import { Building } from '../../src/domain/building-agg/building';
import { Floor } from '../../src/domain/floor';



describe('Integration test floor controller -> service ', function () {
	const sandbox = sinon.createSandbox();

	beforeEach(function() {
		this.timeout(9000);
		Container.reset();
		let buildingSchemaInstance = require("../../src/persistence/schemas/buildingSchema").default;
		Container.set("buildingSchema", buildingSchemaInstance);

		let elevatorSchemaClass = require("../../src/persistence/schemas/elevatorSchema").default;
		Container.set("elevatorSchema", elevatorSchemaClass);

		let floorSchemaInstance = require("../../src/persistence/schemas/floorSchema").default;
		Container.set("floorSchema", floorSchemaInstance);

		let buildingRepoClass = require("../../src/repos/buildingRepo").default;
		let buildingRepoInstance = Container.get(buildingRepoClass);
		Container.set("BuildingRepo", buildingRepoInstance);

		let floorRepoClass = require("../../src/repos/floorRepo").default;
		let floorRepoInstance = Container.get(floorRepoClass);
		Container.set("FloorRepo", floorRepoInstance);

		let elevatorRepoClass = require("../../src/repos/elevatorRepo").default;
		let elevatorRepoInstance = Container.get(elevatorRepoClass);
		Container.set("ElevatorRepo", elevatorRepoInstance);

		let buildingInstance = require("../../src/domain/building-agg/building").Building;
		Container.set("Building", buildingInstance);

		let floorInstance = require("../../src/domain/floor").Floor;
		Container.set("Floor", floorInstance);

		let floorServiceClass = require("../../src/services/floorService").default;
		let floorServiceInstance = Container.get(floorServiceClass);
		Container.set("FloorService", floorServiceInstance);

    });

	afterEach(function() {
		sinon.restore();
		sandbox.restore();
	});

    it('floorController -> floorService integration test using floorRepo and floor stub (createFloor)', async function () {
		// Arrange
        let body = { "buildingId":'Building', "width":10,"length":10,"floorNumber":2,"floorMap":[[]],"description":"Edificio muito alto." };
		let bodyBuildind = { "code":'Building', "maxWidth":10,"maxLength":10,"name":"Edificio lindo","description":"Edificio muito alto." };
        let req: Partial<Request> = {};
		req.body = body;
        let res: Partial<Response> = {
			json: sinon.spy()
        };
		let next: Partial<NextFunction> = () => {};

		let buildingRepoInstance = Container.get("BuildingRepo");
		let floorRepoInstance = Container.get("FloorRepo");
		let floorInstance = Container.get("Floor");

		const buildingRepoStub = sinon.stub(buildingRepoInstance, "findByDomainId").returns(new Promise<Building>((resolve, reject) => {
			resolve(Building.create({
				"code": bodyBuildind.code,
				"maxWidth":bodyBuildind.maxWidth,
				"maxLength":bodyBuildind.maxLength,
				"name":bodyBuildind.name,
				"description":bodyBuildind.description
			}).getValue())
		}));

		const floorRepoStub = sinon.stub(floorRepoInstance, "save").returns(new Promise<Floor>((resolve, reject) => {
			resolve(Floor.create({
				"buildingId": req.body.buildingId,
				"floorNumber": req.body.floorNumber,
				"width":req.body.width,
				"length":req.body.length,
				"floorMap":req.body.floorMap,
				"description":req.body.description
			}).getValue())
		}));

		const floorStub = sinon.stub(floorInstance, "create").returns( Result.ok( {
			"id":"123",
			"buildingId": req.body.buildingId,
			"floorNumber": req.body.floorNumber,
			"width":req.body.width,
			"length":req.body.length,
			"floorMap":req.body.floorMap,
			"description":req.body.description}
		));


		let floorServiceInstance = Container.get('FloorService');
		const ctrl = new FloorController(floorServiceInstance as IFloorService);

		// Act
		await ctrl.createFloor(<Request>req, <Response>res, <NextFunction>next);

		// Assert
		sinon.assert.calledOnce(res.json);
 		sinon.assert.calledWith(res.json, sinon.match({
			"id":"123",
			"buildingId": req.body.buildingId,
			"floorNumber": req.body.floorNumber,
			"width":req.body.width,
			"length":req.body.length,
			"floorMap":req.body.floorMap,
			"description":req.body.description
		}));
		sinon.assert.calledOnce(floorStub);
		sinon.assert.calledOnce(floorRepoStub);
		sinon.assert.calledOnce(buildingRepoStub);
	});

    it('floorController -> floorService integration test using floorRepo and floor stub (updateFloor)', async function () {
		// Arrange
        let body = { "buildingId":'Building', "width":10,"length":10,"floorNumber":2,"floorMap":[[]],"description":"Edificio muito alto." };
		let bodyBuildind = { "code":'Building', "maxWidth":10,"maxLength":10,"name":"Edificio lindo","description":"Edificio muito alto." };
        let req: Partial<Request> = {};
		req.body = body;
        let res: Partial<Response> = {
			json: sinon.spy()
        };
		let next: Partial<NextFunction> = () => {};

		let buildingRepoInstance = Container.get("BuildingRepo");
		let floorRepoInstance = Container.get("FloorRepo");
		let floorInstance = Container.get("Floor");

		const floorRepoStub2 = sinon.stub(floorRepoInstance, "findByDomainId").returns(new Promise<Floor>((resolve, reject) => {
			resolve(Floor.create({
				"buildingId": req.body.buildingId,
				"floorNumber": req.body.floorNumber,
				"width":req.body.width,
				"length":req.body.length,
				"floorMap":req.body.floorMap,
				"description":req.body.description
			}).getValue())
		}));

		const floorRepoStub = sinon.stub(floorRepoInstance, "save").returns(new Promise<Floor>((resolve, reject) => {
			resolve(Floor.create({
				"buildingId": req.body.buildingId,
				"floorNumber": req.body.floorNumber,
				"width":req.body.width,
				"length":req.body.length,
				"floorMap":req.body.floorMap,
				"description":req.body.description
			}).getValue())
		}));


		let floorServiceInstance = Container.get('FloorService');
		const ctrl = new FloorController(floorServiceInstance as IFloorService);

		// Act
		await ctrl.updateFloor(<Request>req, <Response>res, <NextFunction>next);

		// Assert
		sinon.assert.calledOnce(res.json);
 		sinon.assert.calledWith(res.json, sinon.match({
			"buildingId": req.body.buildingId,
			"floorNumber": req.body.floorNumber,
			"width":req.body.width,
			"length":req.body.length,
			"floorMap":req.body.floorMap,
			"description":req.body.description
		}));

		sinon.assert.calledOnce(floorRepoStub);
		sinon.assert.calledOnce(floorRepoStub2);
	});

});


