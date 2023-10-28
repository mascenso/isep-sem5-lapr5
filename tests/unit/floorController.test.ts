import 'reflect-metadata';

import * as sinon from 'sinon';
import { Response, Request, NextFunction } from 'express';
import { Container } from 'typedi';
import { Result } from '../../src/core/logic/Result';
import IFloorService from "../../src/services/IServices/IFloorService";
import FloorController from "../../src/controllers/floorController";
import {IFloorDTO} from '../../src/dto/IFloorDTO';
import { Floor } from '../../src/domain/floor';
import { IBuildingDTO } from '../../src/dto/IBuildingDTO';

describe('Floor controller', function () {
	const sandbox = sinon.createSandbox();

	beforeEach(function() {
		this.timeout(6000);
		Container.reset();
		let floorSchemaInstance = require("../../src/persistence/schemas/floorSchema").default;
		Container.set("floorSchema", floorSchemaInstance);

		let floorRepoClass = require("../../src/repos/floorRepo").default;
		let floorRepoInstance = Container.get(floorRepoClass);
		Container.set("FloorRepo", floorRepoInstance);

		let floorInstance = require("../../src/domain/floor").default;
		Container.set("Floor", floorInstance);

		let floorServiceClass = require("../../src/services/floorService").default;
		let floorServiceInstance = Container.get(floorServiceClass);
		Container.set("FloorService", floorServiceInstance);

    });

	afterEach(function() {
		sinon.restore();
		sandbox.restore();
	});

    it('FloorController unit test using floorService stub (createFloor)', async function () {
		// Arrange
        let body = { "buildingId":'Building', "width":10,"length":10,"floorNumber":2,"floorMap":[[]],"description":"Edificio muito alto." };
        let req: Partial<Request> = {};
		req.body = body;
        let res: Partial<Response> = {
			json: sinon.spy()
        };
		let next: Partial<NextFunction> = () => {};

		let floorServiceInstance = Container.get("FloorService");
		sinon.stub(floorServiceInstance, "createFloor").returns( Result.ok<IFloorDTO>( {
			"id":"123", 
			"buildingId": req.body.buildingId,
			"width":req.body.width,
			"length":req.body.length,
			"floorNumber":req.body.floorNumber,
			"floorMap":req.body.floorMap,
			"description":req.body.description} ));

		const ctrl = new FloorController(floorServiceInstance as IFloorService);

		// Act
		await ctrl.createFloor(<Request>req, <Response>res, <NextFunction>next);

		// Assert
		sinon.assert.calledOnce(res.json);
		sinon.assert.calledWith(res.json, sinon.match({ 
			"id":"123", 
			"buildingId": req.body.buildingId,
			"width":req.body.width,
			"length":req.body.length,
			"floorNumber":req.body.floorNumber,
			"floorMap":req.body.floorMap,
			"description":req.body.description
		}));
	});

	it('FloorController unit test using floorService stub (updateFloor)', async function () {
		// Arrange
        let body = { "buildingId":'Building', "width":10,"length":10,"floorNumber":2,"floorMap":[[]],"description":"Edificio muito alto." };
        let req: Partial<Request> = {};
		req.body = body;
        let res: Partial<Response> = {
			json: sinon.spy()
        };
		let next: Partial<NextFunction> = () => {};

		let floorServiceInstance = Container.get("FloorService");
		sinon.stub(floorServiceInstance, "updateFloor").returns( Result.ok<IFloorDTO>( {
			"id":"123", 
			"buildingId": req.body.buildingId,
			"width":req.body.width,
			"length":req.body.length,
			"floorNumber":req.body.floorNumber,
			"floorMap":req.body.floorMap,
			"description":req.body.description} ));

		const ctrl = new FloorController(floorServiceInstance as IFloorService);

		// Act
		await ctrl.updateFloor(<Request>req, <Response>res, <NextFunction>next);

		// Assert
		sinon.assert.calledOnce(res.json);
		sinon.assert.calledWith(res.json, sinon.match({ 
			"id":"123", 
			"buildingId": req.body.buildingId,
			"width":req.body.width,
			"length":req.body.length,
			"floorNumber":req.body.floorNumber,
			"floorMap":req.body.floorMap,
			"description":req.body.description
		}));
	});

	it('FloorController unit test using floorService stub (addMapToFloor)', async function () {
		// Arrange
        let body = { "buildingId":'Building', "width":10,"length":10,"floorNumber":2,"floorMap":[[]],"description":"Edificio muito alto." };
        let req: Partial<Request> = {};
		req.body = body;
        let res: Partial<Response> = {
			json: sinon.spy()
        };
		let next: Partial<NextFunction> = () => {};

		let floorServiceInstance = Container.get("FloorService");
		sinon.stub(floorServiceInstance, "addMapToFloor").returns( Result.ok<IFloorDTO>( {
			"id":"123", 
			"buildingId": req.body.buildingId,
			"width":req.body.width,
			"length":req.body.length,
			"floorNumber":req.body.floorNumber,
			"floorMap":req.body.floorMap,
			"description":req.body.description} ));

		const ctrl = new FloorController(floorServiceInstance as IFloorService);

		// Act
		await ctrl.addMapToFloor(<Request>req, <Response>res, <NextFunction>next);

		// Assert
		sinon.assert.calledOnce(res.json);
		sinon.assert.calledWith(res.json, sinon.match({ 
			"id":"123", 
			"buildingId": req.body.buildingId,
			"width":req.body.width,
			"length":req.body.length,
			"floorNumber":req.body.floorNumber,
			"floorMap":req.body.floorMap,
			"description":req.body.description
		}));
	});

	it('FloorController unit test using floorService stub (getFloorsAtBuildings)', async function () {
		// Arrange
		let body = { "buildingId":'Building', "width":10,"length":10,"floorNumber":2,"floorMap":[[]],"description":"Edificio muito alto." };
		let query = { "building":"building"}
        let req: Partial<Request> = {};
		req.body = body;
		req.query = query;
        let res: Partial<Response> = {
			json: sinon.spy()
        };
		let next: Partial<NextFunction> = () => {};

		let floorServiceInstance = Container.get("FloorService");
		sinon.stub(floorServiceInstance, "getFloorsAtBuildings").returns( Result.ok<IFloorDTO[]>( [{
			"id":"123", 
			"buildingId": req.body.buildingId,
			"width":req.body.width,
			"length":req.body.length,
			"floorNumber":req.body.floorNumber,
			"floorMap":req.body.floorMap,
			"description":req.body.description}] ));

		const ctrl = new FloorController(floorServiceInstance as IFloorService);

		// Act
		await ctrl.getFloorsAtBuildings(<Request>req, <Response>res, <NextFunction>next);

		// Assert
		sinon.assert.calledOnce(res.json);
		sinon.assert.calledWith(res.json, sinon.match([{
			"id":"123",
			"buildingId": req.body.buildingId,
			"width":req.body.width,
			"length":req.body.length,
			"floorNumber":req.body.floorNumber,
			"floorMap":[[]],
			"description":req.body.description
		}]));
	});

	it('FloorController unit test using floorService stub (getAllFloors)', async function () {
		// Arrange
        let body = { "buildingId":'Building', "width":10,"length":10,"floorNumber":2,"floorMap":[[]],"description":"Edificio muito alto." };
        let req: Partial<Request> = {};
		req.body = body;
        let res: Partial<Response> = {
			json: sinon.spy()
        };
		let next: Partial<NextFunction> = () => {};

		let floorServiceInstance = Container.get("FloorService");
		sinon.stub(floorServiceInstance, "getAllFloors").returns( Result.ok<IFloorDTO[]>( [
			{
				"id":"123", 
				"buildingId": req.body.buildingId,
				"width":req.body.width,
				"length":req.body.length,
				"floorNumber":req.body.floorNumber,
				"floorMap":req.body.floorMap,
				"description":req.body.description},
			{
				"id":"123", 
				"buildingId": req.body.buildingId,
				"width":req.body.width,
				"length":req.body.length,
				"floorNumber":req.body.floorNumber,
				"floorMap":req.body.floorMap,
				"description":req.body.description}
			]));

		const ctrl = new FloorController(floorServiceInstance as IFloorService);

		// Act
		await ctrl.getAllFloors(<Request>req, <Response>res, <NextFunction>next);

		// Assert
		sinon.assert.calledOnce(res.json);
		sinon.assert.calledWith(res.json, sinon.match([
			{
				"id":"123", 
				"buildingId": req.body.buildingId,
				"width":req.body.width,
				"length":req.body.length,
				"floorNumber":req.body.floorNumber,
				"floorMap":req.body.floorMap,
				"description":req.body.description},
			{
				"id":"123", 
				"buildingId": req.body.buildingId,
				"width":req.body.width,
				"length":req.body.length,
				"floorNumber":req.body.floorNumber,
				"floorMap":req.body.floorMap,
				"description":req.body.description}
			]));
	});

	it('FloorController unit test using floorService stub (getBuildingsByMinMaxFloors)', async function () {
		// Arrange
        let body = { "code":'Building', "maxWidth":10,"maxLength":10,"description":"Edificio muito alto.","name":"Edificio muito alto." };
		let query = { "minFloor":"1","maxFloor":"2"}
        let req: Partial<Request> = {};
		req.body = body;
		req.query = query;
        let res: Partial<Response> = {
			json: sinon.spy()
        };
		let next: Partial<NextFunction> = () => {};

		let floorServiceInstance = Container.get("FloorService");
		sinon.stub(floorServiceInstance, "getBuildingsByMinMaxFloors").returns( Result.ok<IBuildingDTO[]>( [{
			"id":"123", 
			"code": req.body.code,
			"maxWidth":req.body.maxWidth,
			"maxLength":req.body.maxLength,
			"name":req.body.name,
			"description":req.body.description}]));

		const ctrl = new FloorController(floorServiceInstance as IFloorService);

		// Act
		await ctrl.getBuildingsByMinMaxFloors(<Request>req, <Response>res, <NextFunction>next);

		// Assert
		sinon.assert.calledOnce(res.json);
		sinon.assert.calledWith(res.json, sinon.match([{ 
			"id":"123", 
			"code": req.body.code,
			"maxWidth":req.body.maxWidth,
			"maxLength":req.body.maxLength,
			"name":req.body.name,
			"description":req.body.description
		}]));
	});
});


